######################################################################
# Program: data01.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 5/28/16
######################################################################
# Notes: 
# 1. I only use the 1995-2004 data because after 2004 geographic 
#    data is omitted from the datasets.
#
######################################################################
library(data.table)
library(plyr)
library(foreign)
library(truncnorm)



for (i in 1995:2004){
  path = paste0("/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/linkco",
                i, "us_den.csv")

  DT = fread(path, header = T , nrows = 1000)

  
  if (i == 2003){
    DT = rename(DT, replace = c("mager41" = "dmage", "bfacil3" = "pldel")) # perhaps just use mager
    DT$dmage = DT$dmage + 13 
    DT$sex = (DT$sex == "M") + 0 
    }
  
  if ( i == 2004){
    DT = rename(DT, replace = c("mager41" = "dmage", "bfacil3" = "pldel"))
    DT$sex = (DT$sex == "M") + 0 # this looks incorrect, but it gets recoded later
  }
  
  keepVars = c("matchs", "biryr", "stoccfipb", "dmage", "ormoth", "pldel",
               "mrace", "dmeduc", "dmar", "dtotord", "dlivord", "mpcb", "nprevist", 
               "birattnd", "clingest", "csex", "dbirwt", 
               "dplural", "fmaps", "vaginal", "vbac", "primac", "repeac", "forcep",
               "vacuum", "anemia", "cardiac", "lung", "diabetes", 
               "herpes", "hemo", "chyper", "phyper", "eclamp", "incervix", 
               "pre4000", "preterm", "renal", "rh", "uterine", "othermr", 
               "tobacco", "cigar", "alcohol", "drink", "wtgain", "induct",
               "monitor", "stimula", "tocol", "ultras", "febrile", "meconium", "rupture", 
               "preplace", "abruptio", "excebld", "seizure", "precip", "prolong", "dysfunc", 
               "breech", "cephalo", "cord", "anesthe", "distress", "otherlb", "hydra",
               "nanemia", "injury", "heart", "circul", "spina", "aged")
  if (i >= 2003){
    keepVars = c("matchs", "dob_yy", "ostate", "dmage", "umhisp", "pldel",
                 "mrace", "umeduc", "mar", "tbo", "lbo", "mpcb", "uprevis", 
                 "attend", "estgest", "sex", "dbwt", 
                 "dplural", "f_apgar5", "ume_vag", "ume_vbac", "ume_primc", "ume_repec", "ume_forcp",
                 "ume_vac", "urf_anemia", "urf_card", "urf_lung", "urf_diab", 
                 "urf_gen", "urf_hemo", "urf_chyper", "urf_phyper", "urf_eclam", "urf_incerv", 
                 "urf_pre4000", "urf_preterm", "urf_renal", "urf_rh", "urf_uterine", "urf_other", 
                 "tobuse", "cigs", "alcohol", "drinks", "wtgain", "uop_induc",
                 "uop_monit", "uop_stiml", "uop_tocol", "uop_ultra", "uld_febr", "uld_meco", "uld_ruptr", 
                 "uld_prepla","uld_abrup", "uld_excbl", "uld_seiz", "uld_precip", "uld_prolg", "uld_dysfn", 
                 "uld_breech", "uld_cephal", "uld_cord", "uld_anest", "uld_distr", "uld_other", "urf_hydr",
                 "uab_anem", "uab_injury", "uca_heart", "uca_circ", "uca_spina", "aged")
  }
  
  DT = DT[, keepVars, with = F]
  ### Make the variables consistent between the two time periods
  if(i >= 2003){
    DT = rename(DT, replace = c("dob_yy" = "biryr", "ostate" = "stoccfipb",
                                "umhisp" = "ormoth", "umeduc" = "dmeduc", "mar" = "dmar",
                                "tbo" = "dtotord", "lbo" = "dlivord", "uprevis" = "nprevist", 
                                "attend" = "birattnd", "estgest" = "clingest",
                                "sex" = "csex", "dbwt" = "dbirwt", "f_apgar5" = "fmaps", 
                                "ume_vag" = "vaginal", "ume_vbac" = "vbac", "ume_primc" = "primac",
                                "ume_repec" = "repeac", "ume_forcp" = "forcep", "ume_vac" = "vacuum",
                                "urf_anemia" = "anemia", "urf_card" = "cardiac", "urf_lung" = "lung", 
                                "urf_diab" = "diabetes", "urf_gen" = "herpes", "urf_hemo" = "hemo",
                                "urf_chyper" = "chyper", "urf_phyper" = "phyper", "urf_eclam" = "eclamp",
                                "urf_incerv" = "incervix", "urf_pre4000" = "pre4000", "urf_preterm" = "preterm",
                                "urf_renal" = "renal", "urf_rh" = "rh", "urf_uterine" = "uterine", 
                                "urf_other" = "othermr", "f_tobaco" = "tobacco", "cigs" = "cigar", "drinks" = "drink",
                                "uop_induc" = "induct", "uop_monit" = "monitor", "uop_stiml" = "stimula", 
                                "uop_tocol" = "tocol", "uop_ultra" ="ultras", "uld_febr" = "febrile", 
                                "uld_meco" = "meconium", "uld_ruptr" = "rupture", 
                                "uld_abrup" = "abruptio", "uld_excbl" = "excebld", "uld_seiz" = "seizure",
                                "uld_precip" = "precip", "uld_prolg" = "prolong", "uld_dysfn" ="dysfunc", 
                                "uld_breech" = "breech", "uld_cephal" = "cephalo", "uld_cord" = "cord",
                                "uld_anest" = "anesthe", "uld_distr" = "distress", "uld_other" = "otherlb", 
                                "uab_anem" = "nanemia", "uab_injury" = "injury", "uca_heart" = "heart", 
                                "uca_circ" = "circul", "urf_hydr" = "hydra", "uca_spina" = "spina", "uld_prepla" = "preplace"))
    
  }
  
  # Need to add hydra, spina, preplace, aged
  
  
  ####Make the state variable human readable:
  #### There are some issues here #####
  if (i < 2003){
    states = c("Alabama", "Alaska","","Arizona", "Arkansas", 
               "California","", "Colorado", "Connecticut", "Delaware", 
               "District of Columbia", "Florida", "Georgia","", "Hawaii",
               "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
               "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
               "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", 
               "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
               "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
               "Pennsylvania","", "Rhode Island", "South Carolina", "South Dakota", 
               "Tennessee", "Texas", "Utah", "Vermont", "Virginia","", "Washington", 
               "West Virginia", "Wisconsin", "Wyoming") 
               #there need to blanks to match the numbering in the description file
    DT$state = states[DT$stoccfipb] }
  else {DT$state = state.name[match(DT$stoccfipb, state.abb)]}
  DT[, stateStr := state]
  DT[, state := as.factor(state)]
  
  
  #Create new variables
  DT[, mort := (matchs == 1) + 0]
  DT[, mwhite := (mrace == 1) + 0]
  DT[, mblack := (mrace == 2) + 0]
  DT[, married := (dmar == 1) + 0]
  DT[, male := (csex == 1) + 0]
  DT[, hyper := (chyper == 1) + 0]
  DT[, midwifedAll := (birattnd == 3 | birattnd == 4) + 0]
  DT[, midwife := (birattnd == 3) + 0]
  
  
  ####################### revalue the missing data ###################
  #99 means missing, set missing values to the median
  missing99 = c("dmeduc", "dmage", "dtotord", "dlivord", "mpcb", 
                "nprevist", "clingest", "fmaps", "cigar", 
                "drink", "wtgain")
  for (col in missing99){
    meanVal = DT[DT[[col]] != 99, mean(DT[[col]])]
    stddev = DT[DT[[col]] != 1, sd(DT[[col]])]
    DT[is.na(get(col)), (col) := rtruncnorm(sum(is.na(get(col))), a = 0, b = 10, mean = meanVal,  sd = stddev) ] 
    DT[DT[[col]] > 10, (col) := rtruncnorm(sum(DT[[col]] > 10), a = 0, b = 10, mean = meanVal,  sd = stddev)] 

  }
  # Dummy for whether or not people have prenatal care
  DT[, dprenat := (mpcb != 0) + 0]
  # Put people with no prenatal care with those that started in the last month
  DT[mpcb == 0, mpcb := 9]

  
  #9 means missing, set missing to not observed
  DT[is.na(birattnd), birattnd := 5]
  DT[birattnd == 9, birattnd := 5] # set missing to other
  missing9 = c("matchs", "pldel", "dmar",  "forcep", "vaginal", "primac", "repeac", "vbac",
               "vacuum", "anemia", "cardiac", "lung", "diabetes", 
               "herpes", "hemo", "chyper", "phyper", "eclamp", "incervix", 
               "pre4000", "preterm", "renal", "rh", "uterine", "othermr", 
               "tobacco", "cigar", "alcohol", "drink", "induct",
               "monitor", "stimula", "tocol", "ultras", "febrile", "meconium", "rupture", 
               "preplace", "abruptio", "excebld", "seizure", "precip", "prolong", "dysfunc", 
               "breech", "cephalo", "cord", "anesthe", "distress", "otherlb", "hydra",
               "nanemia", "injury", "heart", "circul", "spina")
  for (col in missing9){
    
    DT[DT[[col]] == 2, (col) := 0]
    cutoff = DT[DT[[col]] == 1 | DT[[col]] == 0, mean(DT[[col]])]
    stddev = DT[DT[[col]] == 1 | DT[[col]] == 0, sd(DT[[col]])]
    DT[is.na(get(col)), (col) := (rtruncnorm(sum(is.na(get(col))), a = 0, mean = cutoff,  sd = stddev) > cutoff) + 0] 
    DT[DT[[col]] == 9, (col) := (rtruncnorm(sum(DT[[col]] == 9), a = 0, mean = cutoff,  sd = stddev) > cutoff) + 0] 
    DT[DT[[col]] == 8, (col) := (rtruncnorm(sum(DT[[col]] == 8), a = 0, mean = cutoff,  sd = stddev) > cutoff) + 0] 
  }
  DT = DT[primac != 1 & repeac != 1 & vbac != 1]
  
  
  # DT[, numCompPre := diabetes + herpes + hemo + hyper + eclamp + incervix + pre4000 + 
  #      preterm + renal + rh + uterine + othermr ] # this is not all the complications
  # DT[, numCompDur := tocol + febrile + rupture + abruptio + excebld + seizure + precip + 
  #      prolong + dysfunc + breech + cephalo + cord + distress + otherlb]

  ############# here ###################
  
    
  meanValWT = DT[dbirwt != 9999, mean(dbirwt)]
  stddevWT = DT[dbirwt != 9999, sd(dbirwt)]
  DT[dbirwt == 9999, dbirwt := rtruncnorm(sum(is.na(get(col))), a = 0, mean = meanValWT,  sd = stddevWT)] # Look into upper limit
  DT = DT[!is.na(DT$stoccfipb)] #remove missing states
  DT$biryr_factor = as.factor(DT$biryr)
  
  if (i == 1995){
    allbirths = DT
    completed = c(i)
    next
  }
  allbirths = rbind(DT, allbirths)
  completed = append(completed, i)
}

rm(DT)

write.csv(allbirths, "C:\\Josh Taylor\\allbirths.csv")
# write.table(allbirths, "/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/allbirths.csv")
# write.dta(allbirths, "C:\\Josh Taylor\\allbirths.dta")

