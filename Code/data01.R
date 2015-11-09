######################################################################
# Program: data01.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 11/5/15
######################################################################
library(data.table)
library(plyr)
library(foreign)

for (i in 1995:2006){
  path = paste0("C:\\Josh Taylor\\linkco", i, "us_den.csv")
#   path = paste0("/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/linkco", 
#                 i, "us_den.csv")
  
  DT = fread(path, header = T) #, nrows = 1000)
  
  if (i == 2003){DT = rename(DT, replace = c("umagerpt" = "dmage"))}
  if (i >= 2004){DT = rename(DT, replace = c("mager41" = "dmage"))}
  
  keepVars = c("matchs", "biryr", "stoccfipb", "dmage", "ormoth", 
               "mrace", "dmeduc", "dmar", "dtotord", "dlivord", "mpcb", "nprevist", 
              "birattnd", "clingest", "csex", "dbirwt", 
               "dplural", "fmaps", "vaginal", "vbac", "primac", "repeac", "forcep",
               "vacuum", "anemia", "cardiac", "lung", "diabetes", 
               "herpes", "hemo", "chyper", "phyper", "eclamp", "incervix", 
               "pre4000", "preterm", "renal", "rh", "uterine", "othermr", 
               "tobacco", "cigar", "alcohol", "drink", "wtgain", "induct",
               "monitor", "stimula", "tocol", "ultras", "febrile", "meconium", "rupture", 
               "abruptio", "excebld", "seizure", "precip", "prolong", "dysfunc", 
               "breech", "cephalo", "cord", "anesthe", "distress", "otherlb", 
               "nanemia", "injury", "heart", "circul")
  if (i >= 2003){
    keepVars = c("matchs", "dob_yy", "ostate", "dmage", "umhisp", 
                 "mrace", "umeduc", "mar", "tbo", "lbo", "mpcb", "uprevis", 
                 "attend", "estgest", "sex", "dbwt", 
                 "dplural", "f_apgar5", "ume_vag", "ume_vbac", "ume_primc", "ume_repec", "ume_forcp",
                 "ume_vac", "urf_anemia", "urf_card", "urf_lung", "urf_diab", 
                 "urf_gen", "urf_hemo", "urf_chyper", "urf_phyper", "urf_eclam", "urf_incerv", 
                 "urf_pre4000", "urf_preterm", "urf_renal", "urf_rh", "urf_uterine", "urf_other", 
                 "f_tobaco", "cigs", "alcohol", "drinks", "wtgain", "uop_induc",
                 "uop_monit", "uop_stiml", "uop_tocol", "uop_ultra", "uld_febr", "uld_meco", "uld_ruptr", 
                 "uld_abrup", "uld_excbl", "uld_seiz", "uld_precip", "uld_prolg", "uld_dysfn", 
                 "uld_breech", "uld_cephal", "uld_cord", "uld_anest", "uld_distr", "uld_other", 
                 "uab_anem", "uab_injury", "uca_heart", "uca_circ")
  }
  
  
  DT = DT[, keepVars, with = F]
  
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
                                "uca_circ" = "circul"))
    
  }
  
  ####Make the state variable human readable:
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
               "Tennessee", "Texas", "Utah", "Vermont", "virginia","", "Washington", 
               "West Virginia", "Wisconsin", "Wyoming") 
    #there need to blanks to match the numbering in the description file
    DT$state = states[DT$stoccfipb]}
  else{DT$state = state.name[match(DT$stoccfipb, state.abb)]}
  DT$stateStr = DT$state
  DT$state = as.factor(DT$state)
  
  #Create new variables
  DT$mort = (DT$matchs == 1) + 0
  DT$mwhite = (DT$mrace == 1) + 0
  DT$mblack  = (DT$mrace == 2) + 0
  DT$married = (DT$dmar == 1) + 0
  DT$male = (DT$csex == 1) + 0
  DT$hyper = (DT$chyper == 1 | DT$phyper == 1) + 0
  DT$midwifeAll = (DT$birattnd == 3 | DT$birattnd == 4) + 0
  DT$midwife = (DT$birattnd == 3) + 0
  DT$numCompPre = DT$diabetes + DT$herpes + DT$hemo + DT$hyper + DT$eclamp + DT$incervix + DT$pre4000 + 
    DT$preterm + DT$renal + DT$rh + DT$uterine + DT$othermr 
  DT$numCompDur = DT$tocol + DT$febrile + DT$rupture + 
    DT$rupture + DT$abruptio + DT$excebld + DT$seizure + DT$precip + DT$prolong + DT$dysfunc + 
    DT$breech + DT$cephalo + DT$cord + DT$distress + DT$otherlb
  # remove c-sections because midwives can't perform them. Vbacs are off-limits for midwives in some
  # states
  DT[primac != 1 & repeac != 1 & vbac != 1]
  
  
  ##revalue the missing data##
  #99 means missing, set missing values to the median
  missing99 = c("dmeduc", "dmage", "dtotord", "dlivord", "mpcb", 
                "nprevist", "clingest", "fmaps", "cigar", 
                "drink", "wtgain")
  for (col in missing99){
    DT[is.na(get(col)), (col) := median(DT[[col]])]
    DT[DT[[col]] == 99, (col) := median(DT[[col]])]
  }
  
  #9 means missing, set missing to not observed
  DT[is.na(birattnd), birattnd := 5]
  DT[birattnd == 9, birattnd := 5] # set missing to other
  missing9 = c( 'vaginal', 'meconium', 'vbac', 
               'primac', 'repeac', 'forcep', 'vacuum', 
               'anemia', 'cardiac', 'lung', 'diabetes', 
               'herpes', 'hemo', 'eclamp', 'incervix', 'pre4000', 
               'preterm', 'renal', 'rh', 'uterine', 'othermr', 
               'alcohol', 'tobacco', 'induct', 'monitor', 'stimula',
               'tocol', 'ultras', 'febrile', 'rupture', 'abruptio', 
               'excebld', 'seizure', 'precip', 'prolong', 'dysfunc', 
               'breech', 'cephalo', 'cord', 'anesthe', 'distress',
               'otherlb', 'injury', 'heart' )
  for (col in missing9){
    DT[is.na(get(col)), (col) := median(DT[[col]])]
    DT[DT[[col]] == 9, (col) := 0]
    DT[DT[[col]] == 8, (col) := 0]
    DT[DT[[col]] == 2, (col) := 0]
  }
  
  
  DT[dbirwt == 9999, dbirwt := median(DT$dbirwt)]
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

#remove the missing level from the states var.



# write.table(allbirths, "C:\\Josh Taylor\\allbirths.csv")
# write.table(allbirths, "/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/allbirths.csv")
write.dta(allbirths, "C:\\Josh Taylor\\allbirths.dta")

