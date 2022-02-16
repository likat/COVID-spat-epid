library(dplyr)
library(lubridate)
library(pscl)
library(ggplot2)
library(gridExtra)

vaccine <- read.csv("vaccine_dec5.csv", row.names=1)
vaccine <- data.frame(vaccine)
vaccine$date <- ymd(vaccine$date)

# restricting to timeframe when full vaccination is possible
vaccine <- vaccine[vaccine$date>= "2021-01-04", ]
sp2ind <- which(vaccine$date >= "2021-06-21")
vaccine$t <- rep(1:length(unique(vaccine$date)), each=3)
vaccine$s <- sin(2*pi*vaccine$t/18)
vaccine$s2 <- cos(2*pi*vaccine$t/18)
vaccine$s3 <- sin(2*pi*vaccine$t/26)
vaccine$s4 <- cos(2*pi*vaccine$t/26)
vaccine$sp2 <- 0
vaccine$sp2[sp2ind] <- vaccine$t[sp2ind] - sum(unique(vaccine$date) <"2021-06-21")
vaccine$logv0 <- log(vaccine$unvaccinated)
vaccine$logv2 <- log(vaccine$two_dose_count + 0.0001)

## marginal df for vaccine
vaccineall <- vaccine %>% group_by(date) %>% summarise(
  t=mean(t),
  s=mean(s),
  s2=mean(s2),
  s3=mean(s3),
  s4=mean(s4),
  sp2=mean(sp2),
  v0=sum(unvaccinated),
  v2 = sum(two_dose_count),
  logv0 = log(v0),
  logv2 = log(v2+ 0.0001)
)

deathall <- read.csv("Deaths_All_Ages_dec5.csv",row.names=1) %>% data.frame() %>% round()
death2064 <- read.csv("Deaths_20_64_dec5.csv", row.names=1)%>% data.frame()%>% round()
death6574 <- read.csv("Deaths_65_74_dec5.csv", row.names=1)%>% data.frame()%>% round()
death75 <- read.csv("Deaths_75_plus_dec5.csv", row.names=1)%>% data.frame()%>% round()
rownames(deathall) <- 45:92
rownames(death2064) <- 45:92
rownames(death6574) <- 45:92
rownames(death75) <- 45:92

modeldf_all <- bind_cols(deathall, vaccineall)
modeldf_2064 <- bind_cols(death2064, vaccine[vaccine$agegrp=="20-64 years",])
modeldf_6574 <- bind_cols(death6574, vaccine[vaccine$agegrp=="65-74 years",])
modeldf_75 <- bind_cols(death75, vaccine[vaccine$agegrp=="75+ years",])

predicted_time_series <- function(data, vaccinated, title){
  colnames(data)[1:3] <- c("D0", "D1", "D2")
  if(vaccinated){
    mod <- MASS::glm.nb(D2 ~  t + s + s2 + s3 + s4 + sp2 + offset(logv2), data = data)
    mod_fitted <- data
    mod_fitted$D2 <- fitted(mod)
    plot <- ggplot(data, aes(x = date, y = D2)) + geom_line(aes(color = "red")) + 
      geom_line(data = mod_fitted, aes(color = "blue")) + labs(color = "Legend") + 
      scale_color_manual(labels = c("Predicted", "Observed"), values = c("blue", "red")) + 
      ggtitle(title) + scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") + 
      xlab("Date") + theme_bw() + theme(text = element_text(size = 15))
  }
  else{
    mod <- MASS::glm.nb(D0 ~  t + s + s2 + s3 + s4 + sp2 + offset(logv0), data = data)
    mod_fitted <- data
    mod_fitted$D0 <- fitted(mod)
    plot <- ggplot(data, aes(x = date, y = D0)) + geom_line(aes(color = "red")) + 
      geom_line(data = mod_fitted, aes(color = "blue")) + labs(color = "Legend") + 
      scale_color_manual(labels = c("Predicted", "Observed"), values = c("blue", "red")) + 
      ggtitle(title) + scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") + 
      xlab("Date") + theme_bw() + theme(text = element_text(size = 15))
  }
  return(plot)
}

plot1 <- predicted_time_series(modeldf_all, F, "Unvaccinated Deaths, All Ages")
plot2 <- predicted_time_series(modeldf_all, T, "Fully vaccinated Deaths, All Ages")

plot3 <- predicted_time_series(modeldf_2064, F, "Unvaccinated Deaths, Ages 20-64")
plot4 <- predicted_time_series(modeldf_2064, T, "Fully vaccinated Deaths, Ages 20-64")

plot5 <- predicted_time_series(modeldf_6574, F, "Unvaccinated Deaths, Ages 65-74")
plot6 <- predicted_time_series(modeldf_6574, T, "Fully vaccinated Deaths, Ages 65-74")

plot7 <- predicted_time_series(modeldf_75, F, "Unvaccinated Deaths, Ages 75+")
plot8 <- predicted_time_series(modeldf_75, T, "Fully vaccinated Deaths, Ages 75+")

grid.arrange(plot1, plot2, nrow = 2)
grid.arrange(plot3, plot4, nrow = 2)
grid.arrange(plot5, plot6, nrow = 2)
grid.arrange(plot7, plot8, nrow = 2)




























