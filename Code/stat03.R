######################################################################
# Program: stat03.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 11/19/15
######################################################################

#### Look at most extreme states ###########################################


georgiaLR = subset(lowRiskFirst, stateStr == "Georgia") #largest share of midwife deliveries
georgiaHR = subset(highRiskFirst, stateStr == "Georgia")

#remove 'other' midwives and 'other' birth attendants
georgiaLR = subset(subset(georgiaLR, birattnd != 5), birattnd != 4)
georgiaHR = subset(subset(georgiaHR, birattnd != 5), birattnd != 4)


######## Summary statistics ###############################################

#LR
stargazer(georgiaLR, type = "html",
          keep = c("biryr", "male", "mwhite", "dmeduc", "dmage", "married",
                   "mpcb", "nprevist", "clingest", "forcep", "vacuum", "meconium",
                   "monitor", "midwife"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GALR Summary Table.htm")

#HR
stargazer(georgiaHR, type = "html",
          keep = c( "biryr", "male", "mwhite", "dmeduc", "dmage", "married",
                    "mpcb", "nprevist", "clingest", "forcep", "vacuum", "meconium",
                    "monitor", "midwife", "drink", "cigar", "anemia", "cardiac",
                     "lung", "diabetes", "herpes", "hyper", "eclamp", "incervix",
                     "renal", "rh", "breech", "clingest", "dplural", "abruptio",
                     "excebld", "seizure", "precip", "prolong", "dysfunc", "cephalo",
                     "cord", "distress", "uterine", "otherlb", "othermr", 
                     "febrile", "rupture", 'anesthe', "tocol", "meconium",
                     "monitor", "induct", "stimula", "wtgain"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GAHR Summary Table.htm")



############################################################################

#propensity for low-risk mothers
propenLR = glm(midwife ~ biryr_factor  + male + mwhite + dmeduc + 
                 dmage + married + mpcb + nprevist + clingest, 
               family = binomial(link = "logit"), data = georgiaLR)
georgiaLR$propen = propenLR$fitted

#propensity for high-risk mothers
propenHR = glm(midwife ~ biryr_factor  + male + mwhite + dmeduc + 
                 dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                 cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                 renal + rh + breech + clingest + dplural + abruptio +
                 excebld + seizure + precip + prolong + dysfunc + cephalo + 
                 cord + distress + uterine + otherlb + othermr + 
                 febrile + rupture + anesthe + tocol + 
                 monitor + induct + stimula + wtgain, 
               family = binomial(link = "logit"), data = georgiaHR)
georgiaHR$propen = georgiaHR$fitted

stargazer(propenLR, propenHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          omit = c("drink", "cigar", "anemia", "cardiac", "lung", "diabetes",
                   "herpes", "hyper",  "eclamp", "incervix", "renal",
                   "rh", "breech", "dplural", "abruptio",
                   "excebld", "seizure", "precip", "prolong", "dysfunc",
                   "cephalo", "cord", "distress", "uterine", "otherlb",
                   "othermr", "febrile", "rupture", "anesthe", "tocol", 
                   "meconium", "monitor", "induct", "stimula", "wtgain"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\propensityGA.htm")

stargazer(propenLR, propenHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\Extended PropensityGA.htm")


matchLR.out = matchit(midwife ~ biryr_factor + male + mwhite + dmeduc + 
                        dmage + married + mpcb + nprevist + clingest + forcep + vacuum + 
                        meconium + monitor, 
                      data = georgiaLR, method = "nearest", ratio = 1)
matchGALR = match.data(matchLR.out)
write.csv(matchGALR, "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Data\\matchGALR.csv")


sink(file = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GALR Summary", type = "output")
summary(matchLR.out)
sink()
plot(matchLR.out, type = "jitter")
plot(matchLR.out, type = "hist")
pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\lowRiskGABalance.pdf")
plot(matchLR.out, type = "jitter")
plot(matchLR.out, type = "hist")
dev.off()

matchHR.out =  matchit(midwife ~ biryr_factor + male + mwhite + dmeduc + 
                         dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                         cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                         renal + rh + breech + clingest + dplural + abruptio +
                         excebld + seizure + precip + prolong + dysfunc + cephalo + 
                         cord + distress + uterine + otherlb + othermr + forcep + vacuum +
                         febrile + rupture + anesthe + tocol + meconium + 
                         monitor + induct + stimula, 
                       data = georgiaHR, method = "nearest", ratio = 1)
sink(file = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GAHR Summary", type = "output")
summary(matchHR.out)
sink()
plot(matchHR.out, type = "jitter")
plot(matchHR.out, type = "hist")
pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\highRiskGABalance.pdf")
plot(matchHR.out, type = "jitter")
plot(matchHR.out, type = "hist")
dev.off()
matchGAHR = match.data(matchHR.out)
write.csv(matchGAHR, "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Data\\matchGAHR.csv")

####Run the mortality regressions using the matched data ############################

mortLR = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage + married + mpcb + nprevist + forcep + vacuum + clingest + 
               meconium + monitor + wtgain, 
             family = binomial(link = "logit"), data = matchGALR)

#mortality for high-risk mothers
mortHR = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
               cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
               renal + rh + breech + clingest + dplural + abruptio +
               excebld + seizure + precip + prolong + dysfunc + cephalo + 
               cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
               febrile + rupture + anesthe + tocol + meconium + 
               monitor + induct + stimula + wtgain, 
             family = binomial(link = "logit"), data = matchGAHR)


mortLRAll = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage + married + mpcb + nprevist + forcep + vacuum + 
                  clingest + meconium + monitor + wtgain, 
                family = binomial(link = "logit"), data = georgiaLR)

#mortality for high-risk mothers
mortHRAll = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                  cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                  renal + rh + breech + clingest + dplural + abruptio +
                  excebld + seizure + precip + prolong + dysfunc + cephalo + 
                  cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
                  febrile + rupture + anesthe + tocol + meconium + 
                  monitor + wtgain, 
                family = binomial(link = "logit"), data = georgiaHR)


#regression output
stargazer(mortLRAll, mortLR, mortHRAll, mortHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          omit = c("anemia", "cardiac", "lung", "diabetes", "biryr_factor",
                   "herpes", "hyper",  "eclamp", "incervix", "renal",
                   "rh", "breech", "dplural", "abruptio",
                   "excebld", "seizure", "precip", "prolong", "dysfunc",
                   "cephalo", "cord", "distress", "uterine", "otherlb",
                   "othermr", "febrile", "rupture", "anesthe", "tocol",
                   "meconium", "monitor", "induct", "stimula", "wtgain"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GA Mortality.htm")

#regression output
stargazer(mortLRAll, mortLR, mortHRAll, mortHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GA Mortality Extended.htm")


####Run the APGAR regressions using the matched data ############################

fmapsLR = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage + married + mpcb + nprevist + forcep + vacuum + 
               clingest + meconium + monitor + wtgain, 
             data = matchGALR)

#mortality for high-risk mothers
fmapsHR = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
               cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
               renal + rh + breech + clingest + dplural + abruptio +
               excebld + seizure + precip + prolong + dysfunc + cephalo + 
               cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
               febrile + rupture + anesthe + tocol + meconium + 
               monitor + induct + stimula + wtgain, 
             data = matchGAHR)


fmapsLRAll = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage + married + mpcb + nprevist + forcep + vacuum + 
                  clingest + meconium + monitor + wtgain, 
                data = georgiaLR)

#mortality for high-risk mothers
fmapsHRAll = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                  cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                  renal + rh + breech + clingest + dplural + abruptio +
                  excebld + seizure + precip + prolong + dysfunc + cephalo + 
                  cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
                  febrile + rupture + anesthe + tocol + meconium + 
                  monitor + induct + stimula + wtgain, 
                data = georgiaHR)


#regression output
stargazer(fmapsLRAll, fmapsLR, fmapsHRAll, fmapsHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          omit = c("anemia", "cardiac", "lung", "diabetes", "biryr_factor",
                   "herpes", "hyper",  "eclamp", "incervix", "renal",
                   "rh", "breech", "dplural", "abruptio",
                   "excebld", "seizure", "precip", "prolong", "dysfunc",
                   "cephalo", "cord", "distress", "uterine", "otherlb",
                   "othermr", "febrile", "rupture", "anesthe", "tocol",
                   "meconium", "monitor", "induct", "stimula", "wtgain"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GA APGAR.htm")

#regression output
stargazer(fmapsLRAll, fmapsLR, fmapsHRAll, fmapsHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\GA APGAR Extended.htm")
