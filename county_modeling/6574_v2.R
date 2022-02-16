library(dplyr)
library(tidyr)
library(lubridate)
library(rstan)
# library(spdep)
# library(ggplot2)
## TODO
# change modellab
# change agegrplab
# change death file 
# change denominator of mortality rate
options(mc.cores = 4)
tcar = 48

staniters=8000
modellab <- "6574_v2" ## CHANGE THIS 
agegrplab <- "65-74 years" ## CHANGE THIS
death <- readr::read_csv("Fully_Vaccinated_65_74.csv") ## CHANGE THIS

vaccine <- read.csv("vaccine_cleaned.csv", row.names=1)
vaccine$date <- ymd(vaccine$date)

adjmat <- read.csv("mi_countyadj.csv",row.names=1) %>% as.matrix()
fipscounty <- vaccine %>% group_by(county, fips) %>% summarise(n()) %>% dplyr::select(-c(`n()`))
fips_for_adjmat <- fipscounty$fips[order(rownames(adjmat))]

vaccine <- 
  vaccine %>% 
  filter(date >= "2021-01-04" & date <= "2021-12-04") %>% 
  transmute(
    county=county,
    date = date,
    agegrp = agegrp,
    two_dose_count = cumdose2,
    unvaccinated = unvaccinated,
    fips=fips
  )

vaccine$t <- NA
uniquedates <- sort(unique(vaccine$date))
for(i in 1:length(uniquedates)){
  tempind <- which(vaccine$date == uniquedates[i])
  vaccine$t[tempind] <- i
}
sp2ind <- which(vaccine$date >= "2021-06-21")
vaccine$s <- sin(2*pi*vaccine$t/18)
vaccine$s2 <- cos(2*pi*vaccine$t/26)
vaccine$sp2 <- 0
vaccine$sp2[sp2ind] <- vaccine$t[sp2ind] - sum(unique(vaccine$date) <"2021-06-21")
vaccine$logv0 <- log(vaccine$unvaccinated)
vaccine$logv2 <- log(vaccine$two_dose_count + 0.0001)

# covariate matrices that should be the same across models 
# int<lower=0> p; //number of coefficients (int,time, spline, s1, s2)
# int<lower=0> n; // indexed by i; number of counties
# int<lower=0> T; // indexed by t; number of time points
# matrix[n,T] y;      // matrix of outcome; county x time point
# matrix[n,T] time; // time effect
# matrix[n,T] spline; // time effect
# matrix[n,T] s1; // time effect
# matrix[n,T] s2; // time effect
# matrix<lower=0, upper=1>[n,n] W;// adjacency matrix
# vector<lower=0>[n] sumwts; //rowsums of W; number of neighbors for each county
p <- 5
ncty = nrow(adjmat)
timevec = rep(1:48, each = nrow(adjmat))
splinevec = c(rep(0,tcar-max(vaccine$sp2)),1:max(vaccine$sp2))
s1vec = sin(2*pi*timevec/18)
s2vec = cos(2*pi*timevec/26)
time <- matrix(timevec,
               nrow = ncty)
spline <- matrix(rep(splinevec,each = ncty),
                 nrow = ncty)
s1 <- matrix(s1vec,
             nrow = ncty)
s2 <- matrix(s2vec,
             nrow = ncty)
sumwts <- rowSums(adjmat)[order(rownames(adjmat))] %>% as.numeric()
W <- adjmat[order(rownames(adjmat)),order(colnames(adjmat))] %>% as.matrix()

## death cleaning -----
death <- death[,-1]
death <- death %>% gather(date, deaths, -County)
death$deaths <- round(death$deaths)
death$date <- sub(".","", death$date) %>% mdy()
death <- death %>%
  filter(date >= "2021-01-04" & date <= "2021-12-04")
death$t <- NA
uniquedates <- sort(unique(death$date))
for(k in 1:length(uniquedates)){
  tempind <- which(death$date == uniquedates[k])
  death$t[tempind] <- k
}

colnames(death)[1] <- "county"
death <- 
  death  %>% 
  dplyr::select(-c("date"))
vaccine <- 
  vaccine %>% 
  filter(agegrp == agegrplab) %>% 
  mutate(date = ymd(date)) %>% 
  dplyr::select(-c("date"))
modeldf <- 
  right_join(vaccine,death, by = c("t","county")) %>% 
  transmute(
    county = county,
    t=t,
    mortalityrt = log((deaths+1)/(two_dose_count+0.001)) ## CHANGE THIS
  ) %>% 
  arrange(county, t)

## LOCF for missing time values
modeldf$mortalityrt <- modeldf$mortalityrt %>% DescTools::LOCF() 

modeldf <- reshape(modeldf, timevar = "t", idvar=c("county"),direction="wide")

## declare variables for use in the CAR model
y <- as.matrix(modeldf[,-1])

write.csv(modeldf, paste0("modeldf_",modellab,".csv"))
## 
model <- stan(file = "normal_car.stan",
              data = c("p", "ncty", "tcar", "y",
                       "time","spline", "s1","s2",
                       "W","sumwts"),
              pars = c("b", "sigma_u", "sigma_v", "isigma2"),
              iter=staniters,warmup=staniters/2,control=list(adapt_delta=0.99, max_treedepth=13),chains=4)

saveRDS(model, file = paste0("results/",modellab,".rds"))

