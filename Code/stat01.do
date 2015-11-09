use "C:\Josh Taylor\allbirths.dta", clear
drop biryr_factor 
encode stateStr, gen(state_factor)
tab state_factor

logit midwife i.biryr i.state_factor mwhite dmeduc dmage married male /*
  */ mpcb nprevist dbirwt dtotord dlivord anemia cardiac lung diabetes herpes /* 
  */ hyper eclamp incervix pre4000 preterm renal rh drink cigar breech /*
  */ cephalo dplural

teffects psmatch (mort) (midwife i.biryr i.state_factor mwhite dmeduc dmage married /*
  */ mpcb nprevist dbirwt dtotord dlivord anemia cardiac lung diabetes herpes /*
  */ hemo hyper eclamp incervix pre4000 preterm renal rh drink cigar breech /*
  */ cephalo dplural)
