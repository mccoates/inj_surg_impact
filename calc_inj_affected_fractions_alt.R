
## Matthew Coates
## Make some affected fractions for injuries, utilizing only the flagged nature of injuries and level 2 causes

rm(list=ls())
library(data.table)
library(openxlsx)
library(writexl)


d <- data.table(read.xlsx("C:/Users/MattC/Documents/repos/inj_surg_impact/case_fatality.xlsx"))
d <- d[,c("rei_level_1","rei_level_2","rei_level","rei_name","Case.Fatality.Assumption","DCP4.interv1","DCP4.interv2"),with=F]
d <- d[!is.na(rei_name)]
setnames(d,c("Case.Fatality.Assumption"),c("cf"))

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
d <- merge(d[,c("rei_name","cf","DCP4.interv1","DCP4.interv2"),with=F],gmat,by=c("rei_name"),all=T)
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
d <- d[lowest_cause==1 & rei_level==2]
d[is.na(affected_fraction)]

## copy dataset unique to E-N combos for scaling deaths later
toscale <- copy(d[lowest_cause==1 & rei_level==2])

## affected fractions (deaths implied by CF assumptions and E-N-specific incidence) can go over 100% of the cause-specific deaths
## need to rescale such that they do not exceed cause-specific deaths
## can also have duplicates within E-N combos if there are multiple interventions that address
## so derive these separately from the intervention dataset using dataset copied from earlier before melting
adj_deaths <- copy(toscale[,list(affected_fraction=sum(affected_fraction)),by=c("cause_name")])
adj_deaths[,scalar:=1/affected_fraction]

d <- merge(d,adj_deaths[,c("cause_name","scalar"),with=F],by=c("cause_name"),all.x=T)
d[,affected_fraction:=affected_fraction*scalar]

d <- melt(d,id.vars=c(names(d)[!names(d) %in% c("DCP4.interv1","DCP4.interv2")]),value.name="intervention")
d[,variable:=NULL]
d <- d[!is.na(intervention)]

d <- d[,list(affected_fraction=sum(affected_fraction)),by=c("intervention","cause_name","rei_name")]
d2 <- copy(d[,list(affected_fraction=sum(affected_fraction)),by=c("intervention","cause_name")])

write_xlsx(list(d,d2),"C:/Users/MattC/Documents/repos/inj_surg_impact/inj_affected_fractions.xlsx")






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

