######################################################################
# Program: data01.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 11/5/15
######################################################################
library(data.table)
library(plyr)

for (i in 1995:2006){
  path = paste0("C:\\Josh Taylor\\linkco", i, "us_den.csv")
#   path = paste0("/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/linkco", 
#                 i, "us_den.csv")
  
  DT = fread(path, header = T) #, nrows = 1000)
  
  if (i == 2003){DT = rename(DT, replace = c("umagerpt" = "dmage"))}
  if (i >= 2004){DT = rename(DT, replace = c("mager41" = "dmage"))}
  
  if( i < 2003){
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
  }
  else if (i >= 2003){
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
  
  
  
  #remove these lines
  for (v in keepVars){
    if(!is.element(v, names(DT))){
      print(v)
    }
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

  
  
  #TODO: figure out how to do this in a loop (or in a one-liner)
  
  
  #revalue the missing data
  DT$mort = (DT$matchs == 1) + 0
  DT$mwhite = (DT$mrace == 1) + 0
  DT$mblack  = (DT$mrace == 2) + 0
  
  DT[dmeduc == 99, dmeduc := median(DT$dmeduc)]
  DT$married = (DT$dmar == 1) + 0
  DT[dtotord == 99, dtotord := median(DT$dtotord)]
  DT[dlivord == 99, dlivord := median(DT$dlivord)]
  DT[mpcb == 99, mpcb := median(DT$mpcb)]
  DT[nprevist == 99, nprevist := median(DT$nprevist)]
#   DT[dfage == 99, dfage := median(DT$dfage)]
  DT[birattnd == 9, birattnd := 5] # set missing to other
  DT[clingest == 99, clingest := median(DT$clingest)]
  DT$male = (DT$csex == 1) + 0
  DT[dbirwt == 9999, dbirwt := median(DT$dbirwt)]
  DT[fmaps == 99, fmaps := median(DT$fmaps)]
  DT[vaginal == 9, vaginal := 2]
  DT[vaginal == 8, vaginal := 2]
  DT[vaginal == 2, vaginal := 0]
  DT[meconium == 9, meconium := 2]
  DT[meconium == 8, meconium := 2]
  DT[meconium == 2, meconium := 0]
  DT[vbac == 9, vbac := 2]
  DT[vbac == 8, vbac := 2]
  DT[vbac == 2, vbac := 0]
  DT[primac == 9, primac := 2]
  DT[primac == 8, primac := 2]
  DT[primac == 2, primac := 0]
  DT[repeac == 9, repeac := 2]
  DT[repeac == 8, repeac := 2]
  DT[repeac == 2, repeac := 0]
  DT[forcep == 9, forcep := 2]
  DT[forcep == 8, forcep := 2]
  DT[forcep == 2, forcep := 0]
  DT[vacuum == 9, vacuum := 2]
  DT[vacuum == 8, vacuum := 2]
  DT[vacuum == 2, vacuum := 0]
  DT[anemia == 9, anemia := 2]
  DT[anemia == 8, anemia := 2]
  DT[anemia == 2, anemia := 0]
  DT[cardiac == 9, cardiac := 2]
  DT[cardiac == 8, cardiac := 2]
  DT[cardiac == 2, cardiac := 0]
  DT[lung == 9, lung := 2]
  DT[lung == 8, lung := 2]
  DT[lung == 2, lung := 0]
  DT[diabetes == 9, diabetes := 2]
  DT[diabetes == 8, diabetes := 2]
  DT[diabetes == 2, diabetes := 0]
  DT[herpes == 9, herpes := 2]
  DT[herpes == 8, herpes := 2]
  DT[herpes == 2, herpes := 0]
  DT[hemo == 9, hemo := 2]
  DT[hemo == 8, hemo := 2]
  DT[hemo == 2, hemo := 0]
  DT$hyper = (DT$chyper == 1 | DT$phyper == 1) + 0
  DT[eclamp == 9, eclamp := 2]
  DT[eclamp == 8, eclamp := 2]
  DT[eclamp == 2, eclamp := 0]
  DT[incervix == 9, incervix := 2]
  DT[incervix == 8, incervix := 2]
  DT[incervix == 2, incervix := 0]
  DT[pre4000 == 9, pre4000 := 2]
  DT[pre4000 == 8, pre4000 := 2]
  DT[pre4000 == 2, pre4000 := 0]
  DT[preterm == 9, preterm := 2]
  DT[preterm == 8, preterm := 2]
  DT[preterm == 2, preterm := 0]
  DT[renal == 9, renal := 2]
  DT[renal == 8, renal := 2]
  DT[renal == 2, renal := 0]
  DT[rh == 9, rh := 2]
  DT[rh == 8, rh := 2]
  DT[rh == 2, rh := 0]
  DT[uterine == 9, uterine := 2]
  DT[uterine == 8, uterine := 2]
  DT[uterine == 2, uterine := 0]
  DT[othermr == 9, othermr := 2]
  DT[othermr == 8, othermr := 2]
  DT[othermr == 2, othermr := 0]
  DT[alcohol == 9, alcohol := 2]
  DT[alcohol == 8, alcohol := 2]
  DT[alcohol == 2, alcohol := 0]
  DT[cigar == 99, cigar := median(DT$cigar)]
  DT[tobacco == 9, tobacco := 2]
  DT[tobacco == 8, tobacco := 2]
  DT[tobacco == 2, tobacco := 0]  
  DT[drink == 99, drink := median(DT$drink)]
  DT[wtgain == 99, wtgain := median(DT$wtgain)]
  DT[induct == 9, induct := 2]
  DT[induct == 8, induct := 2]
  DT[induct == 2, induct := 0]
  DT[monitor == 9, monitor := 2]
  DT[monitor == 8, monitor := 2]
  DT[monitor == 2, monitor := 0]
  DT[stimula == 9, stimula := 2]
  DT[stimula == 8, stimula := 2]
  DT[stimula == 2, stimula := 0]
  DT[tocol == 9, tocol := 2]
  DT[tocol == 8, tocol := 2]
  DT[tocol == 2, tocol := 0]
  DT[ultras == 9, ultras := 2]
  DT[ultras == 8, ultras := 2]
  DT[ultras == 2, ultras := 0]
  DT[febrile == 9, febrile := 2]
  DT[febrile == 8, febrile := 2]
  DT[febrile == 2, febrile := 0]
  DT[rupture == 9, rupture := 2]
  DT[rupture == 8, rupture := 2]
  DT[rupture == 2, rupture := 0]
  DT[abruptio == 9, abruptio := 2]
  DT[abruptio == 8, abruptio := 2]
  DT[abruptio == 2, abruptio := 0]
  DT[excebld == 9, excebld := 2]
  DT[excebld == 8, excebld := 2]
  DT[excebld == 2, excebld := 0]
  DT[seizure == 9, seizure := 2]
  DT[seizure == 8, seizure := 2]
  DT[seizure == 2, seizure := 0]
  DT[precip == 9, precip := 2]
  DT[precip == 8, precip := 2]
  DT[precip == 2, precip := 0]
  DT[prolong == 9, prolong := 2]
  DT[prolong == 8, prolong := 2]
  DT[prolong == 2, prolong := 0]
  DT[dysfunc == 9, dysfunc := 2]
  DT[dysfunc == 8, dysfunc := 2]
  DT[dysfunc == 2, dysfunc := 0]
  DT[breech == 9, breech := 2]
  DT[breech == 8, breech := 2]
  DT[breech == 2, breech := 0]
  DT[cephalo == 9, cephalo := 2]
  DT[cephalo == 8, cephalo := 2]
  DT[cephalo == 2, cephalo := 0]
  DT[cord == 9, cord := 2]
  DT[cord == 8, cord := 2]
  DT[cord == 2, cord := 0]
  DT[anesthe == 9, anesthe := 2]
  DT[anesthe == 8, anesthe := 2]
  DT[anesthe == 2, anesthe := 0]
  DT[distress == 9, distress := 2]
  DT[distress == 8, distress := 2]
  DT[distress == 2, distress := 0]
  DT[otherlb == 9, otherlb := 2]
  DT[otherlb == 8, otherlb := 2]
  DT[otherlb == 2, otherlb := 0]
  DT[injury == 9, injury := 2]
  DT[injury == 8, injury := 2]
  DT[injury == 2, injury := 0]
  DT[heart == 9, heart := 2]
  DT[heart == 8, heart := 2]
  DT[heart == 2, heart := 0]
  DT$midwifeAll = (DT$birattnd == 3 | DT$birattnd == 4) + 0
  DT$midwife = (DT$birattnd == 3) + 0
  DT$numCompPre = DT$diabetes + DT$herpes + DT$hemo + DT$hyper + DT$eclamp + DT$incervix + DT$pre4000 + 
    DT$preterm + DT$renal + DT$rh + DT$uterine + DT$othermr 
  DT$numCompDur = DT$tocol + DT$febrile + DT$rupture + 
    DT$rupture + DT$abruptio + DT$excebld + DT$seizure + DT$precip + DT$prolong + DT$dysfunc + 
    DT$breech + DT$cephalo + DT$cord + DT$distress + DT$otherlb
  # remove c-sections because midwives can't perform them. Vbacs are off-limits for midwives in som
  # states
  DT[primac != 1 & repeac != 1 & vbac != 1]
  if (i == 1995){
    allbirths = DT
    completed = c(i)
    next
  }
  allbirths = rbind(DT, allbirths)
  completed = append(completed, i)
}

write.table(allbirths, "C:\\Josh Taylor\\allbirths.csv")
# write.table(allbirths, "/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/allbirths.csv")


