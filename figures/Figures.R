library(ggplot2)
library(cowplot)

#Vaccine Data
vaccine=read.csv("vaccine_dec5.csv")
dates=unique(vaccine$date)
unvaccinated=vector("numeric")
fully_vaccinated=vector("numeric")
for(i in 1:length(dates)) {
  unvaccinated[i]=sum(vaccine[vaccine$date==dates[i],"unvaccinated"])
  fully_vaccinated[i]=sum(vaccine[vaccine$date==dates[i],"two_dose_count"])
}
unvaccinated=unvaccinated[-length(unvaccinated)]
fully_vaccinated=fully_vaccinated[-length(fully_vaccinated)]

death_unvacc=read.csv("Unvaccinated_Marginal.csv",header=T)
death_fullvacc=read.csv("Fully_Vaccinated_Marginal_Counts.csv",header=T)
dates2=names(death_unvacc)[3:dim(death_unvacc)[2]]
death_unvaccinated=apply(death_unvacc[,dates2],2,sum)
death_fully_vaccinated=apply(death_fullvacc[,dates2],2,sum)


#Unvaccinated Plots
unvaccinated_plots1=data.frame(count=unvaccinated, week=seq(as.Date("2020/03/10"), as.Date("2021/11/30"), "week"),
                               type = rep("Total Number Unvaccinated", 91))
unvaccinated_plots2=data.frame(count=death_unvaccinated, week=seq(as.Date("2020/03/10"), as.Date("2021/11/30"), "week"),
                               type=rep("Weekly Death Incidence", 91))
png(filename="Unvaccinated_Feb.png",width=7.46*300,height=7.46*300,res=300)
g=ggplot(unvaccinated_plots1, aes(x = week, y = count)) + geom_line() 
g=g+xlab('Time')+ylab('') 
g=g+scale_x_date(date_labels = "%b %Y",date_breaks="3 months")
g=g+ggtitle("Michigan Unvaccinated Population COVID-19 Weekly Time Series")+theme_classic()
g=g+theme(axis.line = element_blank(),panel.border = element_rect(colour = "black", fill=NA,size=1.15))
g=g+facet_grid(rows=vars(type))
g=g+geom_vline(xintercept=as.Date("2021/06/21"),linetype="dashed",colour="blue")
g=g+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
g=g+scale_y_continuous(limits=c(0,8.2e6),breaks=c(0,seq(2e06,8e06,length.out=4)),labels=c("0","2,000,000","4,000,000","6,000,000","8,000,000"))
g=g+theme(strip.text.y=element_text(size=15),axis.text.y=element_text(size=11),
                    axis.title.y=element_text(size=15))

g2=ggplot(unvaccinated_plots2, aes(x = week, y = count)) + geom_line() 
g2=g2+xlab('Time')+ylab('') 
g2=g2+scale_x_date(date_labels = "%b %Y",date_breaks="3 months")
g2=g2+theme_classic()
g2=g2+theme(axis.line = element_blank(),panel.border = element_rect(colour = "black", fill=NA,size=1.15))
g2=g2+facet_grid(rows=vars(type))
g2=g2+geom_vline(xintercept=as.Date("2021/06/21"),linetype="dashed",colour="blue")
g2=g2+ylim(0,1050)
g2=g2+theme(strip.text.y=element_text(size=15),axis.text.y=element_text(size=11),
          axis.text.x=element_text(size=11),axis.title=element_text(size=15))

plot_grid(g,g2,ncol=1,align="v")
dev.off()


#Vaccinated Plots
vaccinated_plots1=data.frame(count=fully_vaccinated, week=seq(as.Date("2020/03/10"), as.Date("2021/11/30"), "week"),
                               type = rep("Cumulative Number Vaccinated", 91))
vaccinated_plots2=data.frame(count=death_fully_vaccinated, week=seq(as.Date("2020/03/10"), as.Date("2021/11/30"), "week"),
                               type=rep("Weekly Death Incidence", 91))
png(filename="Vaccinated_Feb.png",width=7.46*300,height=7.46*300,res=300)
g=ggplot(vaccinated_plots1, aes(x = week, y = count)) + geom_line() 
g=g+xlab('Time')+ylab('') 
g=g+scale_x_date(date_labels = "%b %Y",date_breaks="3 months")
g=g+ggtitle("Michigan Fully Vaccinated Population COVID-19 Weekly Time Series")+theme_classic()
g=g+theme(axis.line = element_blank(),panel.border = element_rect(colour = "black", fill=NA,size=1.15))
g=g+scale_y_continuous(breaks=c(0,seq(2e06,8e06,length.out=4)),labels=c("0","2,000,000","4,000,000","6,000,000","8,000,000"))
g=g+facet_grid(rows=vars(type))
g=g+geom_vline(xintercept=as.Date("2021/06/21"),linetype="dashed",colour="blue")
g=g+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
g=g+scale_y_continuous(limits=c(0,8.2e6),breaks=c(0,seq(2e06,8e06,length.out=4)),labels=c("0","2,000,000","4,000,000","6,000,000","8,000,000"))
g=g+theme(strip.text.y=element_text(size=15),axis.text.y=element_text(size=11),
          axis.title=element_text(size=15))

g2=ggplot(vaccinated_plots2, aes(x=week, y=count))+geom_line() 
g2=g2+xlab('Time')+ylab('') 
g2=g2+scale_x_date(date_labels = "%b %Y",date_breaks="3 months")
g2=g2+theme_classic()
g2=g2+theme(axis.line = element_blank(),panel.border = element_rect(colour = "black", fill=NA,size=1.15))
g2=g2+facet_grid(rows=vars(type))
g2=g2+geom_vline(xintercept=as.Date("2021/06/21"),linetype="dashed",colour="blue")
g2=g2+ylim(0,1050)
g2=g2+theme(strip.text.y=element_text(size=15),axis.text.y=element_text(size=11),
            axis.text.x=element_text(size=11),axis.title=element_text(size=15))

plot_grid(g,g2,ncol=1,align="v")
dev.off()

#Time Series Stratified

death_unvacc_2064=read.csv("Unvaccinated_20_64.csv",header=T)
death_unvacc_6574=read.csv("Unvaccinated_65_74.csv",header=T)
death_unvacc_75=read.csv("Unvaccinated_75_plus.csv",header=T)
dates=names(death_unvacc_2064)[3:dim(death_unvacc_2064)[2]]
death_unvaccinated_2064=apply(death_unvacc_2064[,dates],2,sum)
death_unvaccinated_6574=apply(death_unvacc_6574[,dates],2,sum)
death_unvaccinated_75=apply(death_unvacc_75[,dates],2,sum)

png(filename="Stratified_Feb.png",width=7.46*300,height=7.46*300,res=300)

unvaccinated_plots1=data.frame(count=death_unvaccinated_2064, week=seq(as.Date("2020/03/10"), as.Date("2021/11/30"), "week"),
                               type = rep("Aged 20-64", 91))
unvaccinated_plots2=data.frame(count=death_unvaccinated_6574, week=seq(as.Date("2020/03/10"), as.Date("2021/11/30"), "week"),
                               type=rep("Aged 65-74", 91))
unvaccinated_plots3=data.frame(count=death_unvaccinated_75, week=seq(as.Date("2020/03/10"), as.Date("2021/11/30"), "week"),
                               type=rep("Aged 75 or Older", 91))

g=ggplot(unvaccinated_plots1, aes(x = week, y = count)) + geom_line() 
g=g+xlab("Time")+ylab("Death Incidence") 
g=g+scale_x_date(date_labels = "%b %Y",date_breaks="3 months")
g=g+theme_classic()
g=g+theme(axis.line = element_blank(),panel.border = element_rect(colour = "black", fill=NA,size=1.15))
g=g+facet_grid(rows=vars(type))
g=g+geom_vline(xintercept=as.Date("2021/06/21"),linetype="dashed",colour="blue")
g=g+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
          strip.text.y=element_text(size=15),axis.text.y=element_text(size=11),
          axis.title=element_text(size=15))
g=g+ylim(0,640)

g2=ggplot(unvaccinated_plots2, aes(x = week, y = count)) + geom_line() 
g2=g2+xlab("Time")+ylab("Death Incidence") 
g2=g2+scale_x_date(date_labels = "%b %Y",date_breaks="3 months")
g2=g2+theme_classic()
g2=g2+theme(axis.line = element_blank(),panel.border = element_rect(colour = "black", fill=NA,size=1.15))
g2=g2+facet_grid(rows=vars(type))
g2=g2+geom_vline(xintercept=as.Date("2021/06/21"),linetype="dashed",colour="blue")
g2=g2+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),strip.text.y=element_text(size=15),
            axis.text.y=element_text(size=11),
            axis.title=element_text(size=15))
g2=g2+ylim(0,640)

g3=ggplot(unvaccinated_plots3, aes(x = week, y = count)) + geom_line() 
g3=g3+xlab("Time")+ylab("Death Incidence") 
g3=g3+scale_x_date(date_labels = "%b %Y",date_breaks="3 months")
g3=g3+theme_classic()
g3=g3+theme(axis.line = element_blank(),panel.border = element_rect(colour = "black", fill=NA,size=1.15),strip.text.y=element_text(size=15))
g3=g3+facet_grid(rows=vars(type))
g3=g3+geom_vline(xintercept=as.Date("2021/06/21"),linetype="dashed",colour="blue")
g3=g3+theme(axis.text.x=element_text(size=11),axis.text.y=element_text(size=11),
            axis.title=element_text(size=15))
g3=g3+ylim(0,640)

plot_grid(g,g2,g3,ncol=1,align="v")
dev.off()


library(ggplot2)
date=seq(as.Date("2020/03/02"), as.Date("2021/12/02"),"week")
t=1:91
p=100*c(rep(0,43),((0.12-0)/18)*1:18,rep(0.12,30))
plot_dat=data.frame(t,p)
g=ggplot(plot_dat)+geom_line(aes(x=t,y=p))
g=g+ylab("Percentage of Deaths")+xlab("Date")
g=g+theme_classic()+theme(axis.text=element_text(size=11),axis.title=element_text(size=15))
g=g+scale_x_continuous(breaks=c(1,44,91),labels=date[c(1,44,91)])

png(filename="Assumption2.png",width=7.46*300,height=3.71*300,res=300)
g
dev.off()



