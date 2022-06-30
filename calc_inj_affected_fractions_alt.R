
## Matthew Coates
## Make some affected fractions for injuries, utilizing only the flagged nature of injuries, lowest level causes

rm(list=ls())
library(data.table)
library(openxlsx)
library(writexl)


d <- data.table(read.xlsx("C:/Users/MattC/Documents/repos/inj_surg_impact/Injury Matrix 2_2022_02_03.xlsx",sheet="Case Fatality by Nature of Inju"))
d <- d[,c("rei_level_1","rei_level_2","rei_level","rei_name","death_mechanism","Case.Fatality.Assumption","DCP4.interv1","DCP4.interv2","DCP4.interv3"),with=F]
d <- d[!is.na(rei_name)]
setnames(d,c("Case.Fatality.Assumption"),c("cf"))
## going to add the nondisplaced fractures separately, and assume no mortality effect
ndfrac <- copy(d[rei_name=="Fractures"])
d <- d[death_mechanism==1]

## get inj inc by cause
inc <- data.table(read.xlsx("C:/Users/MattC/Documents/repos/inj_surg_impact/inj_inc_stupid_download.xlsx",startRow = 1,colNames=F))
inc1 <- t(inc[1:10])
for (i in 1:(nrow(inc)/10)) {
  inc1 <- rbind(inc1,t(inc[(i*10+1):(i*10+10)]))
}
inc1 <- data.table(as.data.frame(inc1))
setnames(inc1,c("measure_name","location_name","sex","age_group_name","cause_name","metric_name","year","tot_inc","upper","lower"))
inc1 <- inc1[!is.na(measure_name)]
inc1[,tot_inc:=as.numeric(as.character(tot_inc))]

## merge onto causes
gmat <- fread("C:/Users/MattC/Documents/repos/inj_surg_impact/en_matrix_global_incidence.csv")
gmat <- merge(gmat,inc1[,c("cause_name","tot_inc"),with=F],by=c("cause_name"),all.x=T)
gmat[,Pct_of_Ecode_Resulting_in_Ncode:=Incidence/tot_inc]
d <- merge(d[,c("rei_name","cf","death_mechanism","DCP4.interv1","DCP4.interv2"),with=F],gmat,by=c("rei_name"),all=T)
# d[,affected_fraction:=Pct_of_Ecode_Resulting_in_Ncode*cf]
d[,num_e_end_n_death:=Incidence*cf]

## get inj deaths by cause
deaths <- data.table(read.xlsx("C:/Users/MattC/Documents/repos/inj_surg_impact/inj_deaths_stupid_download.xlsx",startRow = 1,colNames=F))
deaths1 <- t(deaths[1:10])
for (i in 1:(nrow(deaths)/10)) {
  deaths1 <- rbind(deaths1,t(deaths[(i*10+1):(i*10+10)]))
}
deaths1 <- data.table(as.data.frame(deaths1))
setnames(deaths1,c("measure_name","location_name","sex","age_group_name","cause_name","metric_name","year","tot_deaths","upper","lower"))
deaths1 <- deaths1[!is.na(measure_name)]
deaths1[,tot_deaths:=as.numeric(as.character(tot_deaths))]
d <- merge(d,deaths1[,c("cause_name","tot_deaths"),with=F],by=c("cause_name"),all=T)
d[cause_name=="Foreign body in eyes",tot_deaths:=0] ## nonfatal cause according to GBD
d[,affected_fraction:=num_e_end_n_death/tot_deaths]
d[tot_deaths==0,affected_fraction:=0]
d <- d[rei_level==2 & death_mechanism==1 & lowest_cause==1]
d[is.na(affected_fraction)]

## copy dataset unique to E-N combos for scaling deaths later
toscale <- copy(d[rei_level==2])

## affected fractions (deaths implied by CF assumptions and E-N-specific incidence) can go over 100% of the cause-specific deaths
## need to rescale such that they do not exceed cause-specific deaths
## can also have duplicates within E-N combos if there are multiple interventions that address
## so derive these separately from the intervention dataset using dataset copied from earlier before melting
adj_deaths <- copy(toscale[,list(affected_fraction=sum(affected_fraction)),by=c("cause_name","level")])
adj_deaths[,scalar:=1/affected_fraction]

d <- merge(d,adj_deaths[,c("cause_name","scalar","level"),with=F],by=c("cause_name","level"),all.x=T)
d[,affected_fraction:=affected_fraction*scalar]
d[,scalar:=NULL]
d <- d[cause_name!="Adverse effects of medical treatment"]

## there are some pretty rare injuries by nature for some causes that probably aren't too relevant
## we may be fairly skeptical that orthopedic managmenet of injuries from venomous animals or drowning would be useful
## so, we can identify when there are affected fractions less than 5% to check whether they make sense
d[affected_fraction < 0.05]

d <- d[!(cause_name=="Venomous animal contact" & (!rei_name %in% c("Poisoning requiring urgent care")))]
d <- d[!(cause_name %in% c("Conflict and terrorism","Cyclist road injuries","Drowning","Exposure to forces of nature",
                           "Falls","Fire, heat, and hot substances","Foreign body in other body part","Motor vehicle road injuries",
                           "Motorcyclist road injuries","Non-venomous animal contact","Other exposuer to mechanical forces","Other road injuries",
                           "Other transport injuries","Other unintentional injuries","Pedestrian road injuries","Physical violence by firearm",
                           "Physical violence by other means","Physical violence by sharp object","Poisoning by carbon monoxide","Poisoning by other means",
                           "Pulmonary aspiration and foreign body in airway","Self-harm by firearm","Self-harm by other specified means","Unintentional firearm injuries") & affected_fraction < .05)]
d <- d[!cause_name %in% c("Environmental heat and cold exposure","Executions and police conflict","Foreign body in eyes")]


d[cause_name=="Pulmonary aspiration and foreign body in airway",c("cause_name","rei_name","affected_fraction"),with=F]

## rescale now that we've dropped some nature of injury causes that don't quite make sense
adj_deaths <- copy(d[,list(affected_fraction=sum(affected_fraction)),by=c("cause_name","level")])
adj_deaths[,scalar:=1/affected_fraction]
d <- merge(d,adj_deaths[,c("cause_name","scalar","level"),with=F],by=c("cause_name","level"),all.x=T)
d[,affected_fraction:=affected_fraction*scalar]

d <- melt(d,id.vars=c(names(d)[!names(d) %in% c("DCP4.interv1","DCP4.interv2","DCP4.interv3")]),value.name="intervention")
d[,variable:=NULL]
d <- d[!is.na(intervention)]

d <- d[,list(affected_fraction=sum(affected_fraction)),by=c("intervention","cause_name","rei_name")]
d2 <- copy(d[,list(affected_fraction=sum(affected_fraction)),by=c("intervention","cause_name")])

d2 <- d2[order(cause_name,intervention)]

# test <- copy(d2[,list(affected_fraction=sum(affected_fraction)),by=c("cause_name")])
# test

d <- merge(d,unique(gmat[,c("cause_name","level","lowest_cause"),with=F]),by=c("cause_name"),all.x=T)
d2 <- merge(d2,unique(gmat[,c("cause_name","level","lowest_cause"),with=F]),by=c("cause_name"),all.x=T)

d2 <- d2[order(intervention,cause_name)]
d2

## now add the non-displaced fractures
d3 <- copy(gmat[rei_name=="Fractures"])
d3[,affected_fraction:=Incidence/tot_inc*.2*.5] ## assumption of 20% of fractures non-displaced, additional 50% discount for other injuries that may have happened from E-code (not just fracture)
d3[,intervention:=ndfrac$DCP4.interv1]
d3 <- d3[,names(d2),with=F]
d3 <- d3[lowest_cause==1 & affected_fraction > 0]
d3 <- d3[!cause_name %in% c("Venomous animal contact","Pulmonary aspiration and foreign body in airway","Poisoning by carbon monoxide",
                            "Poisoning by other means","Drowning","Environmental heat and cold exposure","Fire, heat, and hot substances","Foreign body in eyes",
                            "Physical violence by firearm","Physical violence by sharp object","Self-harm by firearm","Unintentional firearm injuries")]

## now if we have an organized output, we can do a bit better in terms of formatting for copy-paste into input file
eff <- data.table(read.xlsx("C:/Users/MattC/Documents/repos/inj_surg_impact/Injury Matrix 2_2022_02_03.xlsx",sheet="Intervention to Mechanism"))
eff <- eff[Affected.Fraction=="see other tab (by mech)"]
setnames(eff,"Intervention","intervention")

d4 <- rbind(copy(d2),copy(d3))
d4 <- merge(d4,eff[,c("intervention","Mortality.Effect.Size","Disability.Effect.Size"),with=F],by=c("intervention"),all.x=T)
d4[is.na(Mortality.Effect.Size)]
d4 <- d4[,c("intervention","cause_name","affected_fraction","Mortality.Effect.Size","Disability.Effect.Size"),with=F]

## now instead of doing by single interventions, we'll group
unique(d4$intervention)
d4[intervention %in% c("Acute intracranial pressure relief","Trauma laparotomy","Tube thoracostomy","Using common cure agents for intoxication/poisoning"),intervention:="Emergency care, advanced"]
d4[intervention %in% c("Fracture reduction and placement of external fixator and use of traction for fractures","Urgent orthopedic management of injuries with, e.g., open reduction and internal fixation"),
   intervention:="Acute orthopedic management"]
d4[intervention %in% c("Management of non-displaced fractures"),intervention:="Emergency care, basic"]
unique(d4$intervention)

d4[,unaffected_fraction:=1-affected_fraction]
d4 <- d4[,list(affected_fraction=1-prod(unaffected_fraction),Mortality.Effect.Size=weighted.mean(Mortality.Effect.Size,w=affected_fraction),
               Disability.Effect.Size=weighted.mean(Disability.Effect.Size,w=affected_fraction)),by=c("cause_name","intervention")]
d4 <- d4[order(intervention,cause_name)]

## since the advanced orthopedic stuff is sort of complementary, change effects manually
d4[intervention=="Acute orthopedic management",Mortality.Effect.Size:=.3]
d4[intervention=="Acute orthopedic management",Disability.Effect.Size:=.3]

d5 <- merge(copy(d4),deaths1[,c("cause_name","tot_deaths"),with=F],by=c("cause_name"),all.x=T)
d5[,death_impact:=tot_deaths*affected_fraction*Mortality.Effect.Size]
d5[,death_impact_tot:=sum(death_impact),by=c("intervention")]
d5 <- d5[order(intervention,cause_name)]

write_xlsx(list(d,d2,d3,d5),"C:/Users/MattC/Documents/repos/inj_surg_impact/inj_affected_fractions.xlsx")

write.csv(d4,"C:/Users/MattC/Documents/repos/inj_surg_impact/inj_affected_fractions_input.csv",row.names=F)

# for any given lowest leel cause there is set of injuries by mechanism that contribute to the death
# i from 1 to n + polytrauma + other
# For any given cause we list the injuries--scale half to 2/3 + polytrauma + other
# 
# for n code stuff--if they do sum to 1--collectively exhaustive
# for now, for each cause (e.g. pedestrian road injuries--have Ryan and Assad give us N codes plus multiple 
#                          category that we will include, calculate weighted average case fatality)
# 
# 
# Head injuries, spine injuries, femur fracture, pelvis fracture, hip fracture, 

