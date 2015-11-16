######################################################################
# Program: stat02.R
# Project: Midwives/Infant Mortality
# Author: Josh Taylor
# Last edited: 11/16/15
######################################################################

#### Look at most extreme states ###########################################


newMexicoLR = subset(lowRiskFirst, stateStr == "New Mexico") #largest share of midwife deliveries
newMexicoHR = subset(highRiskFirst, stateStr == "New Mexico")

#propensity for low-risk mothers
propenLR = glm(midwife ~ biryr_factor  + male + mwhite + dmeduc + 
                 dmage + married + mpcb + nprevist + clingest + meconium, 
               family = binomial(link = "logit"), data = newMexicoLR)
newMexicoLR$propen = propenLR$fitted

#propensity for high-risk mothers
propenHR = glm(midwife ~ biryr_factor  + male + mwhite + dmeduc + 
                 dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                 cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                 renal + rh + breech + clingest + dplural + abruptio +
                 excebld + seizure + precip + prolong + dysfunc + cephalo + 
                 cord + distress + uterine + otherlb + othermr + 
                 febrile + rupture + anesthe + tocol + meconium + 
                 monitor + induct + stimula + wtgain, 
               family = binomial(link = "logit"), data = newMexicoHR)
newMexicoHR$propen = newMexicoHR$fitted

stargazer(propenLR, propenHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          omit = c("drink", "cigar", "anemia", "cardiac", "lung", "diabetes",
                   "herpes", "hyper",  "eclamp", "incervix", "renal",
                   "rh", "breech", "dplural", "abruptio",
                   "excebld", "seizure", "precip", "prolong", "dysfunc",
                   "cephalo", "cord", "distress", "uterine", "otherlb",
                   "othermr", "febrile", "rupture", "anesthe", "tocol", 
                   "meconium", "monitor", "induct", "stimula", "wtgain"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\propensityNM.htm")

stargazer(propenLR, propenHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\Extended PropensityNM.htm")


matchLR.out = matchit(midwife ~ biryr_factor + male + mwhite + dmeduc + 
                        dmage + married + mpcb + nprevist + clingest + forcep + vacuum + 
                        meconium + monitor + induct + stimula, 
                      data = newMexicoLR, method = "nearest", ratio = 1)
matchNMLR = match.data(matchLR.out)
write.csv(matchLR, "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Data\\matchNMLR.csv")


sink(file = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\NMLR Summary", type = "output")
summary(matchLR.out)
sink()
plot(matchLR.out, type = "jitter")
plot(matchLR.out, type = "hist")
pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\lowRiskNMBalance.pdf")
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
                       data = newMexicoHR, method = "nearest", ratio = 1)
sink(file = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\NMHR Summary", type = "output")
summary(matchHR.out)
sink()
plot(matchHR.out, type = "jitter")
plot(matchHR.out, type = "hist")
pdf("C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\highRiskNMBalance.pdf")
plot(matchHR.out, type = "jitter")
plot(matchHR.out, type = "hist")
dev.off()
matchNMHR = match.data(matchHR.out)
write.csv(matchHR, "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Data\\matchNMHR.csv")

####Run the mortality regressions using the matched data ############################

mortLR = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage + married + mpcb + nprevist + forcep + vacuum + clingest + 
               meconium + monitor + induct + stimula + wtgain, 
             family = binomial(link = "logit"), data = matchNMLR)

#mortality for high-risk mothers
mortHR = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
               cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
               renal + rh + breech + clingest + dplural + abruptio +
               excebld + seizure + precip + prolong + dysfunc + cephalo + 
               cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
               febrile + rupture + anesthe + tocol + meconium + 
               monitor + induct + stimula + wtgain, 
             family = binomial(link = "logit"), data = matchNMHR)


mortLRAll = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage + married + mpcb + nprevist + forcep + vacuum + 
                  clingest + meconium + monitor + induct + stimula + wtgain, 
                family = binomial(link = "logit"), data = newMexicoLR)

#mortality for high-risk mothers
mortHRAll = glm(mort ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                  cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                  renal + rh + breech + clingest + dplural + abruptio +
                  excebld + seizure + precip + prolong + dysfunc + cephalo + 
                  cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
                  febrile + rupture + anesthe + tocol + meconium + 
                  monitor + induct + stimula + wtgain, 
                family = binomial(link = "logit"), data = newMexicoHR)


#regression output
stargazer(mortLRAll, mortLR, mortHRAll, mortHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          omit = c("anemia", "cardiac", "lung", "diabetes",
                   "herpes", "hyper",  "eclamp", "incervix", "renal",
                   "rh", "breech", "dplural", "abruptio",
                   "excebld", "seizure", "precip", "prolong", "dysfunc",
                   "cephalo", "cord", "distress", "uterine", "otherlb",
                   "othermr", "febrile", "rupture", "anesthe", "tocol",
                   "meconium", "monitor", "induct", "stimula", "wtgain"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\NM Mortality.htm")

#regression output
stargazer(mortLRAll, mortLR, mortHRAll, mortHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\NM Mortality Extended.htm")


####Run the APGAR regressions using the matched data ############################

fmapsLR = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage + married + mpcb + nprevist + forcep + vacuum + 
               clingest + meconium + monitor + induct + stimula + wtgain, 
               data = matchNMLR)

#mortality for high-risk mothers
fmapsHR = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
               dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
               cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
               renal + rh + breech + clingest + dplural + abruptio +
               excebld + seizure + precip + prolong + dysfunc + cephalo + 
               cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
               febrile + rupture + anesthe + tocol + meconium + 
               monitor + induct + stimula + wtgain, 
               data = matchNMHR)


fmapsLRAll = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage + married + mpcb + nprevist + forcep + vacuum + 
                  clingest + meconium + monitor + induct + stimula + wtgain, 
                  data = newMexicoLR)

#mortality for high-risk mothers
fmapsHRAll = lm(fmaps ~ midwife + biryr_factor + male + mwhite + dmeduc + 
                  dmage  + married + mpcb + nprevist +  drink + cigar + anemia +
                  cardiac + lung + diabetes + herpes + hyper + eclamp + incervix +
                  renal + rh + breech + clingest + dplural + abruptio +
                  excebld + seizure + precip + prolong + dysfunc + cephalo + 
                  cord + distress + uterine + otherlb + othermr + forcep + vacuum + 
                  febrile + rupture + anesthe + tocol + meconium + 
                  monitor + induct + stimula + wtgain, 
                  data = newMexicoHR)


#regression output
stargazer(fmapsLRAll, fmapsLR, fmapsHRAll, fmapsHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          omit = c("anemia", "cardiac", "lung", "diabetes",
                   "herpes", "hyper",  "eclamp", "incervix", "renal",
                   "rh", "breech", "dplural", "abruptio",
                   "excebld", "seizure", "precip", "prolong", "dysfunc",
                   "cephalo", "cord", "distress", "uterine", "otherlb",
                   "othermr", "febrile", "rupture", "anesthe", "tocol",
                   "meconium", "monitor", "induct", "stimula", "wtgain"),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\NM APGAR.htm")

#regression output
stargazer(fmapsLRAll, fmapsLR, fmapsHRAll, fmapsHR, type = "html", column.labels = c("Low Risk", "High Risk"),
          column.separate = c(2,2),
          out = "C:\\Users\\jt190\\Box Sync\\Home Folder jt190\\Research\\Midwives\\Charts\\NM APGAR Extended.htm")
