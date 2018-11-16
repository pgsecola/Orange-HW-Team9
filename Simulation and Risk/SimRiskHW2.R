library(readxl)
library(lubridate)
library(dplyr)
library(ks)
library(triangle)
library(nortest)
library(ggplot2)
library(truncnorm)
install.packages('Bernoulli')
library(bern)
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
initial_cost = 1000*initial_cost

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
ntimes = 10000
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
ntimes = 10000
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
  Est.x4 <-rnorm(n=6, mean=mean(combined), sd=sd(combined))
  Est.x5<-rtriangle(3, -0.22 , -0.07, -0.0917)
  Est.x6<-rtriangle(4, 0.02 , 0.06, 0.05)
  x7=c(Est.x4,Est.x5,Est.x6,recursive=TRUE)
  cost2019_norm<-initial_cost*(prod(1+x7))
  initial_exp = leased_acres*acre_cost+seismic_sections*seismic_cost+completion_cost+cost2019_norm
  #calculating oil production/volume
  ip = rlnorm(30, 6, .28)
  decline_rate = runif(30, min = 0.15, max = 0.32)
  #x = rlnorm(15, 6, .28)
  R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
  U <- t(chol(R))
  Both.r <- cbind(standardize(ip), standardize(decline_rate))
  SB.r <- U %*% t(Both.r)
  SB.r <- t(SB.r)
  final.SB.r <- cbind(destandardize(SB.r[,1], ip), destandardize(SB.r[,2], decline_rate))
  #new_ip = exp(ip)
  #final_year_rate = (1-decline_rate)*ip
  ip = final.SB.r[1,1]
  decline_rate = final.SB.r[1,2]
  
  end_year_rate = (1-decline_rate)*ip
  yearly_rates = rep(NA, 15)
  oil_volume = rep(NA, 15) 
  yearly_rates[1] = ip
  new_ip = ip
  for (z in 2:16){
    yearly_rates[z] = new_ip*(1-decline_rate)
    new_ip = yearly_rates[z]
    oil_volume[z-1] = 365*((new_ip+yearly_rates[z-1])/2)
  }
  #print (yearly_rates)
  #print (oil_volume)
  #oil_volume = 365*((final_year_rate+ip)/2)
  
  #calculate oil prices
  oil_prices = read_excel('Analysis_Data.xlsx', sheet = 1, skip = 2)
  price_proj = mapply(function(x,y,z) rtriangle(1,x,y,z), oil_prices$`Low Oil Price`, oil_prices$`High Oil Price`, oil_prices$`AEO2018 Reference`)
  
  #calculate oil revenue
  total_rev = price_proj[1:15]*oil_volume
  nri = rnorm(1, mean = .75, sd = .02)
  
  oil_rev = total_rev*nri
  tax = 0.046
  final_rev = oil_rev*(1-tax)
  
  #calculate yearly profit
  overhead = rtriangle(1, 172000, 279500, 215000)
  final_overhead = rep(overhead,15)
  op_exp = nri = rnorm(15, mean = 2.25, sd = .3)
  year_exp = oil_volume*op_exp+final_overhead
  year_profit = final_rev-year_exp
  vec = rep(1.1,15)
  wacc = vec^seq(1,15)
  all_years = year_profit/wacc
  
  #determine profit for next 15 years
  total = sum(all_years)
  done[i] = total-initial_exp
}

#evaluate results
summary(done)

fifth_perc = quantile(done, probs = .05)
fifth_perc = fifth_perc/1000000
#hist(done/1000)
sd(done)
done_plot=as.data.frame(done/1000000)
ggplot(done_plot,aes(done_plot$done))+
  geom_histogram(breaks=seq(-8,60,by=0.1),fill='skyblue2')+
  geom_vline(aes(xintercept = fifth_perc),col='red',size=0.5) +
  scale_x_continuous(breaks=seq(0,40,5)) +
  ggtitle("NPV Distribution")+   
  xlab("NPV (Millions)")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))



################## HW 3 ##################################
ntimes = 1000000
probs = rep(NA, ntimes)
hydro = rep(NA, ntimes)
reser = rep(NA, ntimes)
for (k in 1:ntimes){
  hydrocarbon = rtruncnorm(1, a=0, b=1, mean = .99, sd = .05)
  hydro[k] = hydrocarbon
  reservoir = rtruncnorm(1, a=0, b=1, mean = .8, sd = .1)
  reser[k] = reservoir
  wells = runif(1, min = 10, max = 30)
  prob_wet = hydrocarbon*reservoir
  bernoulli_dist = rbinom(wells, size = 1, prob = prob_wet)
  wet_wells = sum(bernoulli_dist)
  sample_prob =  wet_wells/(floor(wells))
  probs[k] = sample_prob
}
hist(probs)
summary(probs)

probs_plot=as.data.frame(probs)
ggplot(probs_plot,aes(probs_plot$probs))+
  geom_histogram(breaks=seq(0,1,by=0.04),fill='skyblue2')+
  ggtitle("Probability of a Wet Well")+   
  xlab("Probability")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))


res_plot=as.data.frame(reser)
ggplot(res_plot,aes(res_plot$reser))+
  geom_histogram(breaks=seq(0,1,by=0.01),fill='skyblue2')+
  ggtitle("Probability of a Reservoir")+   
  xlab("Probability")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))


hydro_plot=as.data.frame(hydro)
ggplot(hydro_plot,aes(hydro_plot$hydro))+
  geom_histogram(breaks=seq(0,1,by=0.01),fill='skyblue2')+
  ggtitle("Probability of a Hydrocarbon")+   
  xlab("Probability")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))
