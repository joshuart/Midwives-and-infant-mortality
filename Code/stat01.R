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
library(Matching)
library(data.table)

# allbirths = fread("C:\\Josh Taylor\\allbirths.csv", header = T)


propen = glm(midwife ~ factor(biryr) + factor(stoccfipb) + dmage + 
               mwhite + meduc + mage  + married + mpcb + 
               nprevist + dbirwt + dtotord + livord + 
               anemia + cardiac + lung + diabetes + herpes + 
               hemo + hyper + eclamp + incervix + pre4000 + 
               preterm + renal + rh + drink + cigar + breech + 
               cephalo, family = binomial, data = allbirths)

#propensity score matching
match1 = Match(Y = allbirths$mort, Tr = allbirths$midwife, X = propen$fitted, ties = F)

#exact matches on state and year

match2 = Match(Y = allbirths$mort, Tr = allbirths$midwife, X =cbind(propen$fitted, 
              allbirths$stoccfipb, allbirths$biryr), exact = c(F, T, T))

