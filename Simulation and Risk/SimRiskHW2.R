library(readxl)
library(lubridate)
library(dplyr)
library(ks)
library(triangle)
library(nortest)
library(ggplot2)
simfile = read_excel('Analysis_Data.xlsx', sheet = 2, skip = 2)
as.Date(simfile$Date, origin="1960-01-01")
simfile$year = year(simfile$Date)

#extract only years 1991-2006
recent = filter(simfile, year >= "1990-12-31")
recent = filter(recent, year <= "2006-12-31")
#oil_change = colnames(recent)[5]
#convert columns to numeric type because they were read in as characters
recent$`Arithmetic Return - Crude Oil`=  as.numeric(recent$`Arithmetic Return - Crude Oil`)
recent$`Arithmetic Return - Natural Gas`=  as.numeric(recent$`Arithmetic Return - Natural Gas`)
recent$`Arithmetic Return - Dry Well`=  as.numeric(recent$`Arithmetic Return - Dry Well`)

#get initial costs for 2006
initial_cost = rowMeans(recent[16,2:4])

#All changes combined in one vector of 48 observations
combined = c(recent$`Arithmetic Return - Crude Oil`,recent$`Arithmetic Return - Natural Gas`, recent$`Arithmetic Return - Dry Well`)

#checking for normality
hist(combined, breaks = 10)
shapiro.test(combined)
ks.test(combined)
ad.test(combined)
qqplot(combined)

#create initial distribution

#simulation loop to develop cost for 2019 using kernel density
ntimes = 1000000
cost2019 = rep(NA, ntimes)
Density.x4 <- density(combined, bw="SJ-ste")
for(j in 1:ntimes){
  Est.x4 <- rkde(fhat=kde(combined, h=Density.x4$bw), n=6)
  Est.x5<-rtriangle(3, -0.22 , -0.07,-0.0917)
  Est.x6<-rtriangle(4, 0.02, 0.06, 0.05)
  x7=c(Est.x4,Est.x5,Est.x6,recursive=TRUE)
  cost2019[j]<-initial_cost*(prod(1+x7))
}

#plotting and checking simulation results
summary(cost2019)
hist(cost2019, breaks=100, main='2019 cost estimating', xlab='Average Cost')
cost_sim_1=as.data.frame(cost2019)
ggplot(cost_sim_1,aes(cost_sim_1$cost2019))+
  geom_histogram(breaks=seq(0,20000,by=100),fill='skyblue2')+ 
  ggtitle("2019 Drilling Cost Histogram from Simulation")+   
  xlab("Cost in dollar")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))


#simulation loop to develop cost for 2019 using normal distribution
ntimes = 1000000
cost2019_norm = rep(NA, ntimes)
for(j in 1:ntimes){
  set.seed(j)
  Est.x4 <-rnorm(n=6, mean=mean(combined), sd=sd(combined))
  Est.x5<-rtriangle(3, -0.22 , -0.07, -0.0917)
  Est.x6<-rtriangle(4, 0.02 , 0.06, 0.05)
  x7=c(Est.x4,Est.x5,Est.x6,recursive=TRUE)
  cost2019_norm[j]<-initial_cost*(prod(1+x7))
}

#plotting and checking simulation results
summary(cost2019_norm)
hist(cost2019_norm, breaks=100, main='2019 cost estimating', xlab='Average Cost')
cost_sim_1_norm=as.data.frame(cost2019_norm)
ggplot(cost_sim_1_norm,aes(cost_sim_1_norm$cost2019))+
  geom_histogram(breaks=seq(0,20000,by=100),fill='skyblue2')+ 
  ggtitle("2019 Drilling Cost Distribution")+   
  xlab("Cost ($)")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))

#####################         HW2        ##################################


#using code from class to generate simulation for correlated inputs
R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
U <- t(chol(R))

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

#overall loop to get profit for finding a wet well
ntimes = 100000
done = rep(NA, ntimes)
for(i in 1:ntimes){
  #calculating initial expenses
  leased_acres = rnorm(n=1, mean=600, sd=50)
  acre_cost = 960
  seismic_sections = rnorm(n=1, mean=3, sd=.35)
  seismic_cost = 43000
  completion_cost = rnorm(n=1, mean=390000, sd=50000)
  overhead = rtriangle(1, 172000, 279500, 215000)
  Est.x4 <-rnorm(n=6, mean=mean(combined), sd=sd(combined))
  Est.x5<-rtriangle(3, -0.22 , -0.07, -0.0917)
  Est.x6<-rtriangle(4, 0.02 , 0.06, 0.05)
  x7=c(Est.x4,Est.x5,Est.x6,recursive=TRUE)
  cost2019_norm<-initial_cost*(prod(1+x7))
  initial_exp = leased_acres*acre_cost+seismic_sections*seismic_cost+completion_cost+overhead+cost2019_norm
  
  #calculating oil production/volume
  m = 6
  s = .28
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  ip = rlnorm(15, meanlog = location, sdlog = shape)
  decline_rate = runif(15, min = 0.15, max = 0.32)
  Both.r <- cbind(standardize(ip), standardize(decline_rate))
  SB.r <- U %*% t(Both.r)
  SB.r <- t(SB.r)
  final.SB.r <- cbind(destandardize(SB.r[,1], ip), destandardize(SB.r[,2], decline_rate))
  final_year_rate = (1-decline_rate)*ip
  oil_volume = (365*final_year_rate*ip)/2
  
  #calculate oil prices
  oil_prices = read_excel('Analysis_Data.xlsx', sheet = 1, skip = 2)
  price_proj = mapply(function(x,y,z) rtriangle(1,x,y,z), oil_prices$`Low Oil Price`, oil_prices$`High Oil Price`, oil_prices$`AEO2018 Reference`)
  
  #calculate oil revenue
  total_rev = price_proj[1:15]*oil_volume
  nri = rnorm(1, mean = .75, sd = .02)
  
  oil_rev = total_rev*nri
  tax = 0.06
  final_rev = oil_rev*(1-tax)
  
  #calculate yearly profit
  op_exp = nri = rnorm(15, mean = 2.25, sd = .3)
  year_exp = oil_volume*op_exp
  year_profit = final_rev-year_exp
  vec = rep(1.1,15)
  wacc = vec^seq(1,15)
  all_years = year_profit/wacc
  
  #determine profit for next 15 years
  total = sum(all_years)
  done[i] = total-initial_exp
}

#evaluate results
hist(done)
summary(done)
