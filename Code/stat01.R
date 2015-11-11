######################################################################
# Program: stat01.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 11/5/15
######################################################################

###### Details #######################################################
# Regression I want to run:
#   1. mort ~ covariates + midwife_dummy
#   2. apgar ~ covariates + midwife_dummy
#   3. apgar ~ covariates + midwife_dummy (quantile)
#   4. mort ~ covaritaes + midwife_dummy (preterm)
#   5. injury ~ covariates + midwife_dummy
#   
# Data I need to perform the analysis:
#   1. birth_outcome
#   2. birth_attendent
#   3. state
#   4. year
#   5. race_baby/race_mother/race_father
#   6. sex
#   7. gestation (?)
#   8. birthweight (?)
#   9. plurality
#   10. age_mother/age_mother_recode
#   11. education_mother
#   12. interval_since_last_live_birth
#   13. outcome_last_pregnancy
#   14. health_risks
#   15. c_section
#   16. delivery method
######################################################################
library(MatchIt)
library(data.table)
library(doBy)
library(stargazer)
##### Look only at mothers having their first child ##############################################
firstbirths = subset(allbirths, dtotord == 1)

highRiskFirst = firstbirths[dmage >= 40 | clingest < 37 | anemia == 1 |
                              cardiac == 1 | lung == 1 | dplural > 1 |
                              diabetes == 1 | herpes == 1 | abruptio == 1 |
                              hyper == 1 | eclamp == 1 | excebld == 1 |
                              incervix == 1 | renal == 1 | seizure == 1 |
                              rh == 1 | breech == 1 | precip == 1 | 
                              prolong == 1 | dysfunc == 1 | cephalo == 1 | 
                              cord == 1 | distress == 1 | otherlb == 1 | 
                              othermr == 1 | uterine == 1]

# gestation: https://www.betterhealth.vic.gov.au/health/healthyliving/baby-due-date

lowRiskFirst = firstbirths[dmage < 40 & clingest >= 37 & anemia != 1 &
                             cardiac != 1 & lung != 1 & dplural == 1 &
                             diabetes != 1 & herpes != 1 & abruptio != 1 &
                             hyper != 1 & eclamp != 1 & excebld != 1 &
                             incervix != 1 & renal != 1 & seizure != 1 &
                             rh != 1 & breech != 1 & precip != 1 &
                             prolong != 1 & dysfunc != 1 & cephalo != 1 & 
                             cord != 1 & distress != 1 & otherlb != 1 &
                             othermr != 1 & uterine != 1]

#propensity for low-risk mothers
propenLR = glm(midwife ~ biryr_factor + state + male + mwhite + dmeduc + 
                 dmage + married + mpcb + nprevist +  drink + cigar, 
               family = binomial(link = "logit"), data = lowRiskFirst)

#propensity for high-risk mothers
propenHR = glm(midwife ~ biryr_factor + state + male + mwhite + dmeduc + 
                 dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                 cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                 renal + rh + breech + clingest + dplural + abruptio +
                 excebld + seizure + precip + prolong + dysfunc + cephalo + 
                 cord + distress + uterine + otherlb + othermr, 
               family = binomial(link = "logit"), data = highRiskFirst)

stargazer(propenLR, propenHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\propensity.htm")

matchLR.out = matchit(midwife ~ biryr_factor + state + male + mwhite + dmeduc + 
                    dmage + married + mpcb + nprevist + drink + cigar + forcep + 
                    vacuum, 
                  data = lowRiskFirst, method = "nearest", ratio = 1)

summary(matchLR.out)
plot(matchLR.out, type = "jitter")
plot(matchLR.out, type = "hist")
pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\lowRiskBalance.htm")
plot(matchLR.out, type = "jitter")
plot(matchLR.out, type = "hist")
dev.off()
matchLR = match.data(matchLR.out)

matchHR.out =  matchit(midwife ~ biryr_factor + state + male + mwhite + dmeduc + 
                         dmage  + married + mpcb + nprevist +  drink + cigar + forcep + 
                         vacuum + anemia +
                         cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                         renal + rh + breech + clingest + dplural + abruptio +
                         excebld + seizure + precip + prolong + dysfunc + cephalo + 
                         cord + distress + uterine + otherlb + othermr, 
                       data = highRiskFirst, method = "nearest", ratio = 1)
summary(matchHR.out)
plot(matchHR.out, type = "jitter")
plot(matchHR.out, type = "hist")
pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\highRiskBalance.htm")
plot(matchHR.out, type = "jitter")
plot(matchHR.out, type = "hist")
dev.off()
matchHR = match.data(matchLR.out)

####Run the regressions using the matched data ############################

mortLR = glm(mort ~ midwife + biryr_factor + state + male + mwhite + dmeduc + 
               dmage + married + mpcb + nprevist +  drink + cigar, 
               family = binomial(link = "logit"), data = matchLR)

#propensity for high-risk mothers
mortHR = glm(mort ~ midwife + biryr_factor + state + male + mwhite + dmeduc + 
               dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
               cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
               renal + rh + breech + clingest + dplural + abruptio +
               excebld + seizure + precip + prolong + dysfunc + cephalo + 
               cord + distress + uterine + otherlb + othermr, 
               family = binomial(link = "logit"), data = matchHR)


stargazer(mortLR, mortHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\mortality.htm")













#### Look at most extreme states ###########################################
stateRates = summaryBy(midwife + midwifeAll ~ state + stateStr, data = firstbirths, 
                       keep.names = T)
stateRates = stateRates[order(stateRates$midwife),]

newMexicoLR = subset(lowRiskFirst, stateStr == "New Mexico") #largest share of midwife deliveries
newMexicoHR = subset(highRiskFirst, stateStr == "New Mexico")
missouriLR = subset(lowRiskFirst, stateStr == "Missouri") #smallest share of midwife deliveries
missouriHR = subset(highRiskFirst, stateStr == "Missouri")

alaskaLR = subset(lowRiskFirst, stateStr == "Alaska") #largest diff between midwife and midwifeAll
alaskaHR = subset(highRiskFirst, stateStr == "Alaska")








