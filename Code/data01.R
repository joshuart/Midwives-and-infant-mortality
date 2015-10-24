######################################################################
# Program: data01.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 10/21/15
######################################################################
library(data.table)

for (i in 1995:2010){
  DT = fread("/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/linkco1995us_den.csv", 
                header = T, nrows = 1000)
  keepVars = c("matchs", "idnumber", "biryr", "stoccfipb", "dmage", "ormoth", 
               "mrace", "dmeduc", "dmar", "dtotord", "dlivord", "mpcb", "nprevist", 
               "dfage", "birattnd", "clingest", "gestat", "csex", "dbirwt", 
               "dplural", "fmaps", "vaginal", "vbac", "primac", "repeac", "forcep",
               "vacuum", "mrflag", "anemia", "cardiac", "lung", "diabetes", 
               "herpes", "hemo", "chyper", "phyper", "eclamp", "incervix", 
               "pre4000", "preterm", "renal", "rh", "uterine", "othermr", 
               "tobacco", "cigar", "alcohol", "drink", "wtgain", "induct",
               "monitor", "stimula", "tocol", "ultras", "febrile", "rupture", 
               "abruptio", "excebld", "seizure", "precip", "prolong", "dysfunc", 
               "breech", "cephalo", "cord", "anesthe", "distress", "otherlb", 
               "nanemia", "injury", "heart", "circul")
  DT = DT[, keepVars, with = F]

  
  #TODO: figure out how to do this in a loop (or in a one-liner)
  DT[ormoth == 9, ormoth := median(DT$ormoth)] 
  DT[dmeduc == 99, dmeduc := median(DT$dmeduc)]
  DT[dtotord == 99, dtotord := median(DT$dtotord)]
  
  
  # Recode all the variables where the value of 1 is "yes" and 2 is "no"
  recodeVars = c("dmar" )
  
}

