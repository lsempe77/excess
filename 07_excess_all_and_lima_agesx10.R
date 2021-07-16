library(here)
source(here("Code/00_functions.R"))

db_raw <- read_xlsx("Data/Peru/SINADEF_DATOS_ABIERTOS_19032021.xlsx",
                     skip = 2)


# ========================================================================= #
# applying excess for Lima and Peru in 10-year age groups and closing at 80+
# ========================================================================= #
db_p2 <- db_raw %>% 
  select(Sex = SEXO,
         Age = EDAD, 
         Date = FECHA,
         Month = MES,
         Year = AÑO,
         unit_age = 'TIEMPO EDAD',
         Region = "DEPARTAMENTO DOMICILIO") %>% 
  mutate(Sex = recode(Sex,
                      "MASCULINO" = "m",
                      "FEMENINO" = "f"))

unique(db_p2$unit_age)
unique(db_p2$Region)

# All Peru
db_all <- 
  db_p2 %>%
  mutate(Date = ymd(Date),
         IsoWeek = ISOweek::ISOweek(Date),
         Age = ifelse(unit_age == "AÑOS", Age, "0"),
         Age = as.integer(Age),
         Age = floor(Age / 10)*10,
         Age = ifelse(Age >= 80, 80, Age)) %>% 
  group_by(IsoWeek, Age) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Date = ISOweek2date(paste0(IsoWeek, "-7")),
         Country = "Peru",
         Region = "All") %>% 
  filter(IsoWeek != "2016-W52")

db_lima <- 
  db_p2 %>%
  filter(Region == "LIMA") %>% 
  mutate(Date = ymd(Date),
         IsoWeek = ISOweek::ISOweek(Date),
         Age = ifelse(unit_age == "AÑOS", Age, "0"),
         Age = as.integer(Age),
         Age = floor(Age / 10)*10,
         Age = ifelse(Age >= 80, 80, Age)) %>% 
  group_by(IsoWeek, Age) %>% 
  summarise(Deaths = n()) %>% 
  ungroup() %>% 
  mutate(Date = ISOweek2date(paste0(IsoWeek, "-7")),
         Country = "Peru",
         Region = "Lima") %>% 
  filter(IsoWeek != "2016-W52")

a <- 40
db_all %>% 
  filter(Age == a) %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))

db_lima %>% 
  filter(Age == a) %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))

db_peru <- 
  bind_rows(db_all,
            db_lima)

# ========== #
# Population
# ========== #
# TODO: Lima population not yet included !!!!!

# Obtaining pop estimates from the WPP
pop <- read_xlsx("Data/WPP2019_INT_F03_1_POPULATION_BY_AGE_ANNUAL_BOTH_SEXES.xlsx",
                 skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  filter(Country %in% "Peru", 
         Year >= 2000) %>% 
  gather(-Country, -Year, key = "Age", value = "Pop") %>% 
  mutate(Age = as.integer(Age), 
         Pop = as.numeric(Pop) * 1000) 

unique(pop$Country) %>% sort()

# grouping it in 10-year age intervals, and close in 80
# assigning week 26 to each estimate
pop_all <- 
  pop %>% 
  mutate(Age = floor(Age / 10) * 10,
         Age = ifelse(Age > 80, 80, Age)) %>% 
  group_by(Country, Year, Age) %>% 
  summarise(Pop = sum(Pop),
            Week = 26) %>%
  ungroup()

# Interpolating population estimates to weeks using splines
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# dataframe with weeks by year between 2000 and 2020
db_w <- expand_grid(Year = 2000:2021, Week = 1:52) %>% 
  bind_rows(tibble(Year = c(2004, 2009, 2015, 2020), Week = 53)) %>% 
  arrange(Year, Week) %>% 
  mutate(t = 1:n())

ages <- unique(pop_all$Age)
ctrs <- unique(pop_all$Country)

c <- "Peru"

inters_pop <- NULL

for(c in ctrs){
  pop_temp <- pop_all %>% 
    filter(Country == c)
  for(a in ages){
    
    db_w_temp <- db_w %>% 
      mutate(Country = c,
             Age = a) %>% 
      left_join(pop_temp)
    
    db_w_temp2 <- db_w_temp %>% 
      left_join(interpop(db_w_temp)) %>% 
      mutate(Country = c,
             Age = a)
    
    inters_pop <- inters_pop %>% 
      bind_rows(db_w_temp2)
    
  }
}

unique(inters_pop$Country)

# Visual test
# ~~~~~~~~~~~
c <- "Peru"
a <- 0

inters_pop %>% 
  filter(Country == c,
         Age == a) %>% 
  ggplot()+
  geom_line(aes(t, Pop2), col = "black")+
  geom_point(aes(t, Pop), col = "red")

inters_pop2 <- 
  inters_pop %>% 
  select(-Pop, -t) %>% 
  rename(Pop = Pop2) %>% 
  mutate(Region = "All",
         IsoWeek = paste0(Year,
                          "-W",
                          sprintf("%02d", Week)),
         Date = ISOweek2date(paste0(IsoWeek, "-7")))

# write_rds(inters_pop2, here("Output", "peru_pop_interpol_week_age10.rds"))
# unique(inters_pop2$Country) %>% sort()


# merging mortality and population estimates and saving
# ===================================================== #

# Merging mortality data and population estimates
db_dp <- db_peru %>% 
  left_join(inters_pop2) %>% 
  drop_na()


write_rds(db_de, here("Output", "db_peru_for_baseline_age10.rds"))
db_de <- read_rds(here("Output", "db_peru_for_baseline_age10.rds"))





# ===================================== #
# Formatting data for baseline estimation
# ===================================== #

covid_start <- ymd("2020-03-08")

db_de <- db_dp %>% 
  # estimating exposures in person-weeks and rounding deaths to min 1
  mutate(Exposure = Pop / 52) %>% 
  select(Country, Region, Year, Week, Date, Age, Deaths, Exposure) %>% 
  arrange(Country, Region, Age, Date) %>% 
  group_by(Country, Region, Age) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  # adding sinusoidal terms for seasonality
  mutate(sn52 = sin((2*pi*t)/(52)),
         cs52 = cos((2*pi*t)/(52)),
         # excluding COVID-19 pandemic weeks, starting in April 
         include = ifelse(Date < covid_start, 1, 0),
         include = factor(include)) %>% 
  drop_na()


# visual inspection of weeks to include and exclude
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_de %>% 
  filter(Region == "All",
         Age == 80) %>% 
  ggplot()+
  geom_point(aes(t, Deaths, col = include))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# estimating baseline for each region, sex, and age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# loading packages for baseline estimation
p_load(pkgs_bsl, character.only = TRUE)
select <- dplyr::select
# setting cores use for parallel processing
registerDoParallel(cores = 6)

# creating directories to locally store partial results that do not sync with github
if (!dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}

if (!dir.exists(here("Figures","baseline_perux10"))){
  dir.create(here("Figures","baseline_perux10"))
}

if (!dir.exists(here("Output","baseline_perux10"))){
  dir.create(here("Output","baseline_perux10"))
}

# starting year of observation
rgs <- unique(db_de$Region) %>% sort()
ags <- unique(db_de$Age)
# cts <- c("CHL", "CZE", "DEUTNP", "USA", "KOR")
# r <- "All"
# a <- 0
db_blns_all <- tibble()

for (r in rgs) {
  for (a in ags) {
    
    temp <- db_de %>% 
      filter(Region == r,
             Age == a) %>% 
      select(Year, Week, t, Deaths, Exposure, sn52, cs52, include)
    
    cat(paste(r, a, "\n", sep = "_"))
    
    temp2 <- fit_baseline(temp) %>% 
      mutate(Region = r,
             Age = a,
             Date = ISOweek::ISOweek2date(paste0(Year, "-W", sprintf("%02d",Week), "-7")),
             mx_b = 100000 * Baseline / Exposure,
             mx_b_u = 100000 * up / Exposure,
             mx_b_l = 100000 * lp / Exposure,
             mx_d = 100000 * Deaths / Exposure) 
    
    ## plots of estimates
    ## ~~~~~~~~~~~~~~~~~~
    temp2 %>%
      ggplot()+
      geom_vline(xintercept = ymd("2020-04-03"), col = "#b80c09", alpha = 0.1, size = 5)+
      geom_line(aes(Date, mx_d), size = 0.4)+
      geom_ribbon(aes(Date, ymin = mx_b_l, ymax = mx_b_u), fill = "#01BAEF", alpha = 0.25)+
      geom_line(aes(Date, mx_b), col = "#01BAEF", alpha = 0.9, size = 0.6)+
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y")+
      labs(title = paste0(r, "_", a))+
      theme_bw()+
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=11),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))+
      ggsave(paste0("Figures/baseline_perux10/", r, "_", a, ".png"), dpi = 300, width = 6, height = 4)
    
    db_blns_all <- bind_rows(db_blns_all, temp2)
  }
}
detach(package:MASS)

write_csv(db_blns_all, here("Output", "baseline_perux10.csv"))
db_blns_all <- read_csv(here("Output", "baseline_perux10.csv"))

# ============================
# Excess mortality computation
# ============================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Three different excess constructions:
# 1) all excess
# 2) positive excess
# 3) epidemic excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db2 <- 
  db_blns_all %>% 
  as_tibble() %>% 
  mutate(Epi_per = ifelse(Deaths >= up, 1, 0),
         Excess_epi = ifelse(Epi_per == 1, Deaths - Baseline, 0),
         Excess_pos = ifelse(Deaths > Baseline, Deaths - Baseline, 0),
         Excess = Deaths - Baseline,
         Age = as.character(Age))

# Excess mortality since week 8 in 2020 (March 1) 
db_sum <- 
  db2 %>% 
  filter(Date >= covid_start) %>% 
  group_by(Region, Date) %>% 
  summarise(Baseline = sum(Baseline),
            Excess_epi = sum(Excess_epi),
            Excess_pos = sum(Excess_pos),
            Excess = sum(Excess),
            pscore = (Baseline + Excess_epi) / Baseline) %>% 
  ungroup()

# cumulative excess deaths by age starting in Week 8
cum_age <- 
  db2 %>% 
  arrange(Date) %>%
  filter(Date >= covid_start) %>% 
  group_by(Region, Age) %>% 
  mutate(CumEpi = cumsum(Excess_epi),
         CumExc = cumsum(Excess),
         CumPos = cumsum(Excess_pos),
         Exposure = cumsum(Exposure)) %>% 
  arrange(Region, Age, Date) %>% 
  select(Region, Age, Date, CumEpi, CumExc, CumPos, Exposure) %>% 
  ungroup()

# cumulative excess deaths for all ages starting in Week 8
cum <- 
  cum_age %>% 
  group_by(Region, Date) %>% 
  summarise(CumEpi = sum(CumEpi),
            CumExc = sum(CumExc),
            CumPos = sum(CumPos),
            Exposure = sum(Exposure)) %>% 
  arrange(Region, Date) %>% 
  ungroup() %>% 
  mutate(Age = "TOT")

write_csv(cum_age, "Output/cumulative_excess_age_2020_2021_mx_peX10.csv")
# write_csv(cum, "Output/cumulative_excess_all_ages_2020_2021_mx_peX10.csv")

last_dates <- 
  cum_age %>% 
  group_by(Region) %>% 
  filter(Date == max(Date)) %>% 
  select(Region, Date) %>% 
  unique()

# Summarizing total excess during 2020

unique(cum_age$Date)

cum_age %>% 
  filter(Date == "2021-01-03") %>% 
  select(Age, Date, CumEpi, CumExc, CumPos) %>% 
  rename(Excess = CumEpi) %>% 
  group_by(Age) %>% 
  summarise(sum(Excess),
            sum(CumExc),
            sum(CumPos))

cum_age %>% 
  filter(Date == "2021-01-03") %>% 
  rename(Excess = CumEpi) %>% 
  summarise(sum(Excess))

pe3 %>% 
  summarise(sum(Excess))









