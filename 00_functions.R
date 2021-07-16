Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

# install pacman to streamline further package installation
if(!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package pacman not found")
}

library(pacman)

# Required CRAN packages
pkgs <- c("tidyverse",
          "here",
          "lubridate",
          "readxl",
          "vroom",
          "ungroup",
          "HMDHFDplus",
          "parallel",
          "parallelsugar",
          "ISOweek",
          "scales",
          "osfr")


# required packages for baseline estimation
pkgs_bsl <- c("stats",
              "splines",
              "MASS",
              "gnm",
              'doParallel',
              'foreach')


# Install required CRAN packages if not available yet
if(!sum(!p_isinstalled(c(pkgs, pkgs_bsl)))==0) {
  p_install(
    package = pkgs[!p_isinstalled(pkgs)],
    character.only = TRUE
  )
}

# loading basic packages
p_load(pkgs, character.only = TRUE)

# Reuired github packages
packages_git <- c("DemoTools","parallelsugar")

# install from github if necessary
if (!p_isinstalled("DemoTools")) {
  library(remotes)
  install_github("timriffe/DemoTools")
}

if (!p_isinstalled("parallelsugar")){
  library(remotes)
  install_github("nathanvan/parallelsugar")
}
# Load the required CRAN/github packages
p_load(packages_git, character.only = TRUE)


# Weekly mortality interpolation
interpop <- function(db){
  ys <- db %>% drop_na() %>% pull(t)
  ps <- db %>% drop_na() %>% pull(Pop)
  # smoothing using cubic splines
  ws <- db %>% pull(t)
  md2 <- smooth.spline(x = ys, y = ps)
  inter_pop <- tibble(t = ws,
                      Pop2 = predict(md2, ws)$y)
  return(inter_pop)
}

# Extrapolation of mortality for 2021
exterpop <- function(db){
  ys <- 2000:2020
  ps <- db %>% drop_na() %>% pull(Pop)
  # smoothing using cubic splines
  yss <- 2000:2021
  md2 <- smooth.spline(x = ys, y = ps)
  exter_pop <- tibble(Year = yss,
                      Pop2 = predict(md2, yss)$y)
  return(exter_pop)
}

# ======================================================= #
# functions for excess mortality estimation
# ======================================================= #

# function for bootstrapping
# ======================================================= #

boot_pi <- function(model, odata, pdata, n, p) {
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(2020)
  seeds <- round(runif(n, 1, 1000), 0)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(Baseline = predict(model, newdata = pdata, type = "response"),
                    lp = boot_ci[, 1],
                    up = boot_ci[, 2]))
}

# function for fitting model for each Region, sex, and age
# ======================================================= #

fit_baseline <- function(db2) {
  # db2 <- temp
  skip_to_next <- F
  # # data to include in the model
  db_bline <- db2 %>%
    filter(include == 1)

  no_sea2 = gnm(Deaths ~ ns(t, 3) + offset(log(Exposure)),
                data = db_bline,
                family=poisson(link="log"))

  sea2 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(Exposure)),
             data = db_bline,
             family=poisson(link="log"))

  # If seasonal is better than non-seasonal model
  if (no_sea2$aic - sea2$aic >= 6) {
    # evaluating for inclusion of overdispersion parameter for seasonal model
    # Poisson model (without overdispersion)
    base_po <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)),
                   family = poisson, data = db_bline)

    # Negative binomial (accounting for overdispersion)
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)),
                                data = db_bline), silent = T)
    # statistical significance of overdispersion parameter
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    # Conversion of the model (if it does not reach the limit of alterations)
    converged <- length(base_nb$th.warn) == 0

    # If the NB do not converge
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign) | !converged) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }

    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      # if NB converged and has better fit than the Poisson
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)),
                           data = db_bline)
    } else {
      # if NB did not converge or has no better fit than the Poisson
      base <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)),
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)),
                   family = poisson, data = db_bline)

    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)),
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    converged <- length(base_nb$th.warn) == 0

    if (class(base_nb)[1] == "try-error" | is.na(ov_sign) | !converged) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)),
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)),
                  family = poisson, data = db_bline)
    }
  }

  # predicting values and 95% prediction intervals using bootstrapping
  # (2000 iterations)
  tryCatch({
    db3 <- cbind(db2,
                 boot_pi(base, db_bline, db2, 2000, 0.95))
  }, error=function(e){ skip_to_next <<- TRUE})

  if(skip_to_next) { next }

  return(db3)
}

