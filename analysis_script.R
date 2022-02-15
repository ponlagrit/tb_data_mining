library(here)
library(epicalc)
library(tidyverse)
rm(list=ls())
ccex<-readRDS("sample_frame.rds")

pt<-unique(ccex,by="PIDGEN") %>% group_by(status,strata) %>% summarise(number = n())
pt<-as.data.frame(pt)
ptc<-spread(data=pt,key=status,value=number,fill=0)
ptc<-ptc[ptc$case<10,]
lrm<-ptc[ptc$case>0&ptc$control>0&!is.na(ptc$case)&!is.na(ptc$control),]
mta<-lrm %>% group_by(case,control) %>% summarise(number = n()) %>% as.data.frame()
mta$cum.case<-mta$case*mta$number
mta$cum.control<-mta$control*mta$number
head(mta,22)

ccmat<-unique(ccex[strata %in% lrm$strata,],by="PIDGEN") %>% group_by(SEX,ag) %>% 
  summarise(mat=length(unique(strata)),total =n(),
            case=sum(status=="case"),control=sum(status=="control")) %>%
  as.data.frame()
print(ccmat)

ccex$status<-ccex$status=="case"

run1<-data.frame(run=1:length(unique(ccex$DIAGCODE)),DIAGCODE=unique(ccex$DIAGCODE))
run1

cror<-data.frame()
for(i in 1:nrow(run1)){
  upd<-NULL
  lcc<-ccex
  lcc$ex<-lcc$DIAGCODE==run1[i,2]
  lcc<-lcc[order(-ex),]
  lcc<-unique(lcc,by="PIDGEN")
  upd$dx<-run1[i,2]
  upd$a<-nrow(lcc[status==TRUE & ex==TRUE,])
  upd$b<-nrow(lcc[status==FALSE & ex==TRUE,])
  upd$c<-nrow(lcc[status==TRUE & ex==FALSE,])
  upd$d<-nrow(lcc[status==FALSE & ex==FALSE,])
  upd$cror<-upd$a*upd$d/(upd$b*upd$c)
  upd<-as.data.frame(upd)
  cror<-rbind(cror,upd)
}

run2<-cbind(run=1:10,cror %>% arrange(-a,-cror) %>% head(10))

ror<-data.frame()

for(i in 1:nrow(run2)){
  upd<-NULL
  lcc<-ccex
  lcc$ex<-lcc$DIAGCODE==run2[i,2]
  lcc<-lcc[order(-ex),]
  lcc<-unique(lcc,by="PIDGEN")
  upd$dx<-run2[i,2]
  upd$a<-run2$a[i]
  upd$b<-run2$b[i]
  upd$c<-run2$c[i]
  upd$d<-run2$d[i]
  clogit1 <- clogit(status ~ ex+strata(strata),data=lcc) 
  cof<-summary(clogit1)
  upd$or<-cof$conf.int[1]
  upd$lo95<-cof$conf.int[3]
  upd$up95<-cof$conf.int[4]
  upd$pv<-cof$coefficients[5]
  upd$pdq1<-round(as.numeric(summary(lcc$weeks_p)[2]),2)
  upd$pdm<-round(as.numeric(summary(lcc$weeks_p)[3]),2)
  upd$pdq2<-round(as.numeric(summary(lcc$weeks_p)[5]),2)
  upd<-as.data.frame(upd)
  ror<-rbind(ror,upd)
}
