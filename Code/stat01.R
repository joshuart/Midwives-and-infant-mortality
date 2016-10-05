######################################################################
# Program: stat01.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 9/19/16
######################################################################

###### Details #######################################################
# Regression I want to run:
#   1. mort ~ covariates + midwife_dummy
#   2. apgar ~ covariates + midwife_dummy
#   3. apgar ~ covariates + midwife_dummy (quantile)
#   4. mort ~ covaritaes + midwife_dummy (preterm)
#   5. injury ~ covariates + midwife_dummy
#   # Also include death before 28 days and
# Data I need to perform the analysis:
#   1. birth_outcome
#   2. birth_attendent
#   3. state
#   4. year
#   5. race_baby/race_mother/race_father
#   6. sex
#   7. gestation 
#   8. birthweight 
#   9. plurality
#   10. age_mother/age_mother_recode
#   11. education_mother
#   12. interval_since_last_live_birth
#   13. outcome_last_pregnancy
#   14. health_risks
#   15. c_section
#   16. delivery method
#   Note: Add birth month and birth week day dummies
######################################################################
rm(list = ls())
library(data.table)
library(doBy)
library(stargazer)
library(ggplot2)
library(maps)
library(plyr)
##### Look only at mothers having their first child ##############################################
allbirths = fread("/Volumes/Seagate Data Drive/Research/Midwives:Doctors Outcomes/Infant Mortality/Data/allbirths.csv")
firstbirths = allbirths[dtotord == 1]
# write.csv(firstbirths,"C:\\Josh Taylor\\firstbirths.csv"inst )
rm(allbirths)

states <- map_data("state")
stateRates = summaryBy(midwife + midwifeAll + dplural ~ state + stateStr, data = firstbirths, 
                       FUN = c(mean, "sum")) 

stateRates[, `:=` (midwife.sum = NULL, midwifeAll.sum = NULL,  dplural.mean = NULL)]
stateRates = rename(stateRates, replace = c("midwife.mean" = "midwife", 
                                            "midwifeAll.mean" = "midwifeAll", 
                                            "dplural.sum" = "births"))

stateRates = stateRates[order(stateRates$midwife),]
stateRates[, `:=` (births = births/1000, region = tolower(stateStr))]
stateMerge = merge(states, stateRates, sort = FALSE, by = "region")
stateMerge <- stateMerge[order(stateMerge$order), ]

qplot(long, lat, data = stateMerge, group = group, fill = midwife,
      geom="polygon",
      main = "Portion of Births Delivered by Midwives in the U.S.: 1995 - 2004")
ggsave("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\midwifery by state.pdf",width = 11, height = 8.5)

qplot(long, lat, data = stateMerge, group = group, fill = births,
      geom="polygon",
      main = "Thousands of Births in the U.S.: 1995-2004", scipen = 10000)
ggsave("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\births by state.pdf",width = 11, height = 8.5)


# Should I add birth weight to the risk factor list?
# Should I leave spina bifida on the list?

# "pldel", "dmar",  "forcep", "vaginal", "primac", "repeac", "vbac",
# "vacuum", "anemia", "cardiac", "lung", "diabetes", 
# "herpes", "hemo", "chyper", "phyper", "eclamp", "incervix", 
# "pre4000", "preterm", "renal", "rh", "uterine", "othermr", 
# "tobacco", "cigar", "alcohol", "drink", "induct",
# "monitor", "stimula", "tocol", "ultras", "febrile", "meconium", "rupture", 
# "preplace", "abruptio", "excebld", "seizure", "precip", "prolong", "dysfunc", 
# "breech", "cephalo", "cord", "anesthe", "distress", "otherlb", "hydra",
# "nanemia", "injury", "heart", "circul", "spina"


highRiskFirst = firstbirths[dmage >= 40 | clingest < 37 | anemia == 1 | hemo == 1 |
                              cardiac == 1 | lung == 1 | dplural > 1 | tobacco == 1 |
                              diabetes == 1 | herpes == 1 | abruptio == 1 | hydra == 1 |
                              hyper == 1 | eclamp == 1 | excebld == 1 | preplace == 1 |
                              incervix == 1 | renal == 1 | seizure == 1 | spina == 1 |
                              rh == 1 | breech == 1 | precip == 1 | 
                              prolong == 1 | dysfunc == 1 | cephalo == 1 | 
                              cord == 1 | distress == 1 | otherlb == 1 | 
                              othermr == 1 | uterine == 1 | cigar > 0 |
                              drink > 0 | febrile == 1 | rupture == 1 | 
                              anesthe == 1 | tocol == 1 | stimula == 1 |
                              induct == 1 | dmage < 20]

# gestation: https://www.betterhealth.vic.gov.au/health/healthyliving/baby-due-date

lowRiskFirst = firstbirths[dmage < 40 & clingest >= 37 & anemia != 1 &
                             cardiac != 1 & lung != 1 & dplural == 1 &
                             diabetes != 1 & herpes != 1 & abruptio != 1 &
                             hyper != 1 & eclamp != 1 & excebld != 1 &
                             incervix != 1 & renal != 1 & seizure != 1 &
                             rh != 1 & breech != 1 & precip != 1 &
                             prolong != 1 & dysfunc != 1 & cephalo != 1 & 
                             cord != 1 & distress != 1 & otherlb != 1 &
                             othermr != 1 & uterine != 1 & febrile != 1 &
                             rupture != 1 & anesthe != 1 & tocol != 1 &
                             stimula != 1 & induct != 1 & cigar == 0 & 
                             drink == 0 & dmage >= 20]
rm(firstbirths)


#### Old Code ###################
# library(MatchIt)
# library(data.table)
# library(doBy)
# library(stargazer)
# library(ggplot2)
# library(maps)
# library(plyr)
# ##### Look only at mothers having their first child ##############################################
# allbirths = fread("C:\\Josh Taylor\\allbirths.csv")
# firstbirths = subset(allbirths, dtotord == 1)
# # write.csv(firstbirths,"C:\\Josh Taylor\\firstbirths.csv" )
# rm(allbirths)
# 
# states <- map_data("state")
# stateRates = summaryBy(midwife + midwifeAll + dplural ~ state + stateStr, data = firstbirths, 
#                        FUN = c(mean, "sum"))
# 
# stateRates$midwife.sum = stateRates$midwifeAll.sum = stateRates$dplural.mean = NULL
# stateRates = rename(stateRates, replace = c("midwife.mean" = "midwife", 
#                                             "midwifeAll.mean" = "midwifeAll", 
#                                             "dplural.sum" = "births"))
# 
# stateRates = stateRates[order(stateRates$midwife),]
# stateRates$births = stateRates$births/1000
# stateRates$region = tolower(stateRates$stateStr)
# stateMerge = merge(states, stateRates, sort = FALSE, by = "region")
# stateMerge <- stateMerge[order(stateMerge$order), ]
# 
# qplot(long, lat, data = stateMerge, group = group, fill = midwife,
#       geom="polygon",
#       main = "Portion of Births Delivered by Midwives in the U.S.: 1995 - 2004")
# ggsave("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\midwifery by state.pdf",width = 11, height = 8.5)
# 
# qplot(long, lat, data = stateMerge, group = group, fill = births,
#       geom="polygon",
#       main = "Thousands of Births in the U.S.: 1995-2004", scipen = 10000)
# ggsave("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\births by state.pdf",width = 11, height = 8.5)
# 
# 
# 
# highRiskFirst = firstbirths[dmage >= 40 | clingest < 37 | anemia == 1 |
#                               cardiac == 1 | lung == 1 | dplural > 1 |
#                               diabetes == 1 | herpes == 1 | abruptio == 1 |
#                               hyper == 1 | eclamp == 1 | excebld == 1 |
#                               incervix == 1 | renal == 1 | seizure == 1 |
#                               rh == 1 | breech == 1 | precip == 1 | 
#                               prolong == 1 | dysfunc == 1 | cephalo == 1 | 
#                               cord == 1 | distress == 1 | otherlb == 1 | 
#                               othermr == 1 | uterine == 1 | cigar > 0 |
#                               drink > 0 | febrile == 1 | rupture == 1 | 
#                               anesthe == 1 | tocol == 1 | stimula == 1 |
#                               induct == 1 | dmage < 20]
# 
# # gestation: https://www.betterhealth.vic.gov.au/health/healthyliving/baby-due-date
# 
# lowRiskFirst = firstbirths[dmage < 40 & clingest >= 37 & anemia != 1 &
#                              cardiac != 1 & lung != 1 & dplural == 1 &
#                              diabetes != 1 & herpes != 1 & abruptio != 1 &
#                              hyper != 1 & eclamp != 1 & excebld != 1 &
#                              incervix != 1 & renal != 1 & seizure != 1 &
#                              rh != 1 & breech != 1 & precip != 1 &
#                              prolong != 1 & dysfunc != 1 & cephalo != 1 & 
#                              cord != 1 & distress != 1 & otherlb != 1 &
#                              othermr != 1 & uterine != 1 & febrile != 1 &
#                              rupture != 1 & anesthe != 1 & tocol != 1 &
#                              stimula != 1 & induct != 1 & cigar == 0 & 
#                              drink == 0 & dmage >= 20]
# rm(firstbirths)








# #propensity for low-risk mothers
# propenLR = glm(midwife ~ biryr_factor + state + male + mwhite + dmeduc + 
#                  dmage + married + mpcb + nprevist + clingest + meconium + 
#                  monitor, 
#                family = binomial(link = "logit"), data = lowRiskFirst)
# lowRiskFirst$propen = propenLR$fitted
# 
# #propensity for high-risk mothers
# propenHR = glm(midwife ~ biryr_factor + state + male + mwhite + dmeduc + 
#                  dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
#                  cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
#                  renal + rh + breech + clingest + dplural + abruptio +
#                  excebld + seizure + precip + prolong + dysfunc + cephalo + 
#                  cord + distress + uterine + otherlb + othermr + 
#                  febrile + rupture + anesthe + tocol + meconium + monitor +
#                  induct + stimula, 
#                family = binomial(link = "logit"), data = highRiskFirst)
# highRiskFirst$propen = propenHR$fitted
# 
# stargazer(propenLR, propenHR, type = "html", column.labels = c("Low Risk", "High Risk"),
#           omit = c("state","drink", "cigar", "anemia", "cardiac", "lung", "diabetes",
#                    "herpes", "hyper",  "eclamp", "incervix", "renal",
#                    "rh", "breech", "dplural", "abruptio",
#                    "excebld", "seizure", "precip", "prolong", "dysfunc",
#                    "cephalo", "cord", "distress", "uterine", "otherlb",
#                    "othermr", "febrile", "rupture", "anesthe", "tocol",
#                    "meconium", "induct", "stimula"),
#           out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\propensity.htm")
# 
# stargazer(propenLR, propenHR, type = "html", column.labels = c("Low Risk", "High Risk"),
#           omit = "state",
#           out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\Extended Propensity.htm")
# 
# 
# matchLR.out = matchit(midwife ~ biryr_factor + state + propen + forcep + vacuum, 
#                       data = lowRiskFirst, method = "nearest", ratio = 1)
# matchLR = match.data(matchLR.out)
# write.csv(matchLR, "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Data\\matchLR.csv")
# 
# 
# 
# summary(matchLR.out)
# plot(matchLR.out, type = "jitter")
# plot(matchLR.out, type = "hist")
# pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\lowRiskBalance.htm")
# plot(matchLR.out, type = "jitter")
# plot(matchLR.out, type = "hist")
# dev.off()
# 
# matchHR.out =  matchit(midwife ~ biryr_factor + state + propen + forcep + vacuum, 
#                        data = highRiskFirst, method = "nearest", ratio = 1)
# summary(matchHR.out)
# plot(matchHR.out, type = "jitter")
# plot(matchHR.out, type = "hist")
# pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\highRiskBalance.htm")
# plot(matchHR.out, type = "jitter")
# plot(matchHR.out, type = "hist")
# dev.off()
# matchHR = match.data(matchHR.out)
# write.csv(matchHR, "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Data\\matchHR.csv")
# 
# ####Run the regressions using the matched data ############################
# 
# mortLR = glm(mort ~ midwife + biryr_factor + state + male + mwhite + dmeduc + 
#                dmage + married + mpcb + nprevist + forcep + vacuum + 
#                meconium + wtgain, 
#              family = binomial(link = "logit"), data = matchLR)
# 
# #propensity for high-risk mothers
# mortHR = glm(mort ~ midwife + biryr_factor + state + male + mwhite + dmeduc + 
#                dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
#                cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
#                renal + rh + breech + clingest + dplural + abruptio +
#                excebld + seizure + precip + prolong + dysfunc + cephalo + 
#                cord + distress + uterine + otherlb + othermr + forcep + 
#                vacuum + febrile + rupture + anesthe + tocol + meconium + 
#                wtgain + induct + stimula, 
#              family = binomial(link = "logit"), data = matchHR)
# 
# 
# mortLRAll = glm(mort ~ midwife + biryr_factor + state + male + mwhite + dmeduc + 
#                   dmage + married + mpcb + nprevist + forcep + vacuum + 
#                   meconium + wtgain, 
#                 family = binomial(link = "logit"), data = lowRiskFirst)
# 
# #propensity for high-risk mothers
# mortHRAll = glm(mort ~ midwife + biryr_factor + state + male + mwhite + dmeduc + 
#                   dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
#                   cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
#                   renal + rh + breech + clingest + dplural + abruptio +
#                   excebld + seizure + precip + prolong + dysfunc + cephalo + 
#                   cord + distress + uterine + otherlb + othermr + forcep +
#                   vacuum + febrile + rupture + anesthe + tocol + meconium +
#                   wtgain + induct + stimula, 
#                 family = binomial(link = "logit"), data = highRiskFirst)
# 
# stargazer(mortLRAll, mortLR, mortHRAll, mortHR, type = "html", column.labels = c("Low Risk", "High Risk"),
#           column.separate = c(2,2),
#           omit = c("state", "anemia", "cardiac", "lung", "diabetes",
#                    "herpes", "hyper",  "eclamp", "incervix", "renal",
#                    "rh", "breech", "dplural", "abruptio",
#                    "excebld", "seizure", "precip", "prolong", "dysfunc",
#                    "cephalo", "cord", "distress", "uterine", "otherlb",
#                    "othermr", "febrile", "rupture", "anesthe", "tocol",
#                    "meconium", "induct", "stimula", "wtgain"),
#           out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\mortality.htm")
# 




########## Questions left to answer ##########
# 1. Should I include msa? (1995-2002 doesn't contian this data only some major cities)


























