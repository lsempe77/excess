grViz("
digraph dot {

  # a 'graph' statement
  graph [overlap = true,layout=dot]

node [shape = box]
  Population; MortRates; SINADEF; COVID;


node [shape = diamond]
   NegBinomReg;LogitReg;

node [shape = circle]

  DeathsReg;ExcessReg;Completeness;ExcessUNREG;ExcessTOTAL;TotalDeaths

  # several 'edge' statements

SINADEF->NegBinomReg
Population->NegBinomReg->ExcessReg
SINADEF->DeathsReg

SINADEF->LogitReg
Population->LogitReg
MortRates->LogitReg->Completeness

MortRates->ExcessUNREG
Completeness->ExcessUNREG

COVID->ExcessTOTAL
ExcessReg->ExcessTOTAL
ExcessUNREG->ExcessTOTAL
DeathsReg->TotalDeaths
ExcessTOTAL->TotalDeaths
}
",height=500)

