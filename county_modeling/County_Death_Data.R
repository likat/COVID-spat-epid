
library(tidyverse)

#Read Death Data
death=read.csv("Death_county_120221.csv",header=T)

death_Michigan=death %>% filter(Province_State=="Michigan") %>%
                         subset(!is.na(FIPS) & !(FIPS %in% c(80026,90026))) %>%
                         select(Admin2,"X3.2.20":"X11.30.21") %>%
                         rename(County=Admin2) %>%
                         mutate(County=paste(County,"County")) 

County=death_Michigan$County
death_Michigan=death_Michigan %>% select(starts_with("X"))
nw=dim(death_Michigan)[2]%/%7 #Number of Weeks
death_Michigan=death_Michigan[,c(1,2+7*(1:nw))]
death_Michigan_mat=matrix(0,dim(death_Michigan)[1],nw)
for(i in 1:dim(death_Michigan)[1]) {
  leading_week=lead(as.numeric(death_Michigan[i,]))
  lagging_week=lag(as.numeric(death_Michigan[i,]))
  leading_week=leading_week[!is.na(leading_week)]
  lagging_week=lagging_week[!is.na(lagging_week)]
  death_Michigan_mat[i,]=leading_week-lagging_week
}
death_Michigan_mat=pmax(death_Michigan_mat,0)
dates=names(death_Michigan)[2:length(names(death_Michigan))]

#Time Series for Each Vaccination/Age Group
Group_Ages=function(ages) {
  ages_out=ages
  ages_out[ages %in% c("Under 1 year","1-4 Years","5-14 Years","15-24 Years")]="Under 20"
  ages_out[ages %in% c("25-34 Years","35-44 Years","45-54 Years","55-64 Years")]="20-64"
  ages_out[ages %in% c("65-74 Years")]="65-74"
  ages_out[ages %in% c("75-84 Years","85 Years and Over")]="75+"
  ages_out[ages %in% c("All Ages")]="Total"
  return(ages_out)
}

distribution=read.csv("Age_Distribution.csv",header=T)
weeks=distribution$End.Week[as.Date("03/02/2020","%m/%d/%Y") <= as.Date(distribution$End.Week,"%m/%d/%Y") 
                            & as.Date(distribution$End.Week,"%m/%d/%Y")  <= as.Date("11/30/2021","%m/%d/%Y")]
distribution=distribution %>% filter(End.Week %in% weeks) %>% filter(Sex=="All Sex") %>%
  mutate(Age_Clean=Group_Ages(Age.Group),Week=rep(1:length(dates),each=12))
death_counts_by_age=distribution %>% group_by(Week,Age_Clean) %>%
  summarize(death_counts=sum(COVID.19.Deaths)) %>%
  pivot_wider(names_from=Age_Clean,values_from=death_counts) %>%
  select(-`Under 20`) %>%
  ungroup() %>% 
  mutate(Prop20_64=`20-64`/Total,Prop65_74=`65-74`/Total,Prop75plus=`75+`/Total) 

prop_age20_64=death_counts_by_age$Prop20_64
prop_age65_74=death_counts_by_age$Prop65_74
prop_age75_plus=death_counts_by_age$Prop75plus

death_Michigan20_64=t(death_Michigan_mat)*prop_age20_64
death_Michigan65_74=t(death_Michigan_mat)*prop_age65_74
death_Michigan75_plus=t(death_Michigan_mat)*prop_age75_plus

sum(t(death_Michigan20_64)[82,]+t(death_Michigan65_74)[82,]+t(death_Michigan75_plus)[82,])

#20-64
prop0=c(rep(1,43),1-((1-0.68)/18)*1:18,rep(0.68,30))
prop1=c(rep(0,43),((0.28-0)/18)*1:18,rep(0.28,30))
prop2=1-prop0-prop1
D0_20_64=t(prop0*death_Michigan20_64)
D1_20_64=t(prop1*death_Michigan20_64)
D2_20_64=t(prop2*death_Michigan20_64)

#65-74
prop0=c(rep(1,43),1-((1-0.60)/18)*1:18,rep(0.60,30))
prop1=c(rep(0,43),((0.31-0)/18)*1:18,rep(0.31,30))
prop2=1-prop0-prop1
D0_65_74=t(prop0*death_Michigan65_74)
D1_65_74=t(prop1*death_Michigan65_74)
D2_65_74=t(prop2*death_Michigan65_74)

#75+
prop0=c(rep(1,43),1-((1-0.53)/18)*1:18,rep(0.53,30))
prop1=c(rep(0,43),((0.35-0)/18)*1:18,rep(0.35,30))
prop2=1-prop0-prop1
D0_75_plus=t(prop0*death_Michigan75_plus)
D1_75_plus=t(prop1*death_Michigan75_plus)
D2_75_plus=t(prop2*death_Michigan75_plus)

#D0,D1,D2
D0=D0_20_64+D0_65_74+D0_75_plus
D1=D1_20_64+D1_65_74+D1_75_plus
D2=D2_20_64+D2_65_74+D2_75_plus

D0=cbind(County,as.data.frame(D0))
D2=cbind(County,as.data.frame(D2))
names(D0)=c("County",dates)
names(D2)=c("County",dates)

D0_20_64=cbind(County,as.data.frame(D0_20_64))
D2_20_64=cbind(County,as.data.frame(D2_20_64))
names(D0_20_64)=c("County",dates)
names(D2_20_64)=c("County",dates)

D0_65_74=cbind(County,as.data.frame(D0_65_74))
D2_65_74=cbind(County,as.data.frame(D2_65_74))
names(D0_65_74)=c("County",dates)
names(D2_65_74)=c("County",dates)

D0_75_plus=cbind(County,as.data.frame(D0_75_plus))
D2_75_plus=cbind(County,as.data.frame(D2_75_plus))
names(D0_75_plus)=c("County",dates)
names(D2_75_plus)=c("County",dates)

write.csv(D0,"Unvaccinated_Marginal.csv")
write.csv(D2,"Vaccinated_Marginal.csv")

write.csv(D0_20_64,"Unvaccinated_20_64.csv")
write.csv(D0_65_74,"Unvaccinated_65_74.csv")
write.csv(D0_75_plus,"Unvaccinated_75_plus.csv")

write.csv(D2_20_64,"Fully_Vaccinated_20_64.csv")
write.csv(D2_65_74,"Fully_Vaccinated_65_74.csv")
write.csv(D2_75_plus,"Fully_Vaccinated_75_plus.csv")

D0=read.csv("Unvaccinated_Marginal.csv")
D2=read.csv("Vaccinated_Marginal.csv")



