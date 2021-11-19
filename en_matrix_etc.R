## Matthew Coates
## Code on injury external causes versus nature of injury

rm(list=ls())
library(data.table)
library(openxlsx)

datadir <- "C:/Users/MattC/Dropbox (Personal)/hms_dropbox_backup/BCEPS/inj/"

## read in GBD 2019 injury hierarchy
## this hierarchy is from FairChoices severity work--could be some errors, but will double check
h <- readRDS("C:/Users/MattC/Documents/repos/inj_surg_impact/dws_table2019.RDS")
## output just inj to paste into drive doc
h <- h[order(hierarchy2019)]
write.csv(h[level_1=="Injuries",c("hierarchy2019","level_1","level_2","level_3","level_4","level","lowest_cause",
                                  "cause_name"),with=F],"C:/Users/MattC/Documents/repos/inj_surg_impact/inj_cause_hierarchy.csv",row.names=F)

## n-code hierarchy just has a couple levels--extracting from GBD compare
nh <- data.table(read.xlsx("C:/Users/MattC/Documents/repos/inj_surg_impact/n_code_hierarchy.xlsx"))


fls <- dir(datadir,pattern="csv")

d <- list()
for (fl in fls) {
  d[[fl]] <- fread(paste0(datadir,fl))
}
d <- rbindlist(d)

if (length(unique(d$rei_name[!d$rei_name %in% nh$rei_name])) > 0) stop("issue")


## first, save E-N matrix for global, all ages, both sexes (we can check validity of this later)
en <- copy(d[measure_name=="Incidence" & location_name=="Global" & sex_name=="Both" & age_name=="All Ages" & metric_name=="Number"])
en <- unique(en[,c("cause_name","rei_name","val"),with=F])
setnames(en,"val","Incidence")
en <- en[order(cause_name,rei_name)]
en <- merge(en,h[,c("cause_name","hierarchy2019","level_1","level_2","level_3","level_4",
                    "lowest_cause","level"),with=F],by=c("cause_name"),all.x=T)
if (any(is.na(en$hierarchy2019))) stop("missing")
en <- merge(en,nh,by=c("rei_name"),all.x=T)
if (any(is.na(en$rei_level_1))) stop("missing")
en <- en[order(hierarchy2019,rei_order)]
en <- en[cause_name!="All causes"]
en <- en[,c("level_1","level_2","level_3","level_4","level","lowest_cause","cause_name",
            "rei_level_1","rei_level_2","rei_level","rei_name","Incidence"),with=F]
en[,Pct_of_Ecode_Resulting_in_Ncode:=Incidence/sum(Incidence)*100,by=c("cause_name","rei_level")]
en[,Pct_of_Ncode_from_lowest_Ecodes:=Incidence/sum(Incidence)*100,by=c("rei_name","lowest_cause")]
en[lowest_cause==0,Pct_of_Ncode_from_lowest_Ecodes:=NA]

test <- copy(en[,list(Pct_of_Ecode_Resulting_in_Ncode=sum(Pct_of_Ecode_Resulting_in_Ncode)),by=c("cause_name","rei_level")])
test <- copy(en[lowest_cause==1,list(Pct_of_Ncode_from_lowest_Ecodes=sum(Pct_of_Ncode_from_lowest_Ecodes)),by=c("rei_name")])

write.csv(en,"C:/Users/MattC/Documents/repos/inj_surg_impact/en_matrix_global_incidence.csv",row.names=F)



