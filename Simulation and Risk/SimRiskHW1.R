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

#simfile[year >= "1990-01-01"]
#simfile[simfile$year == 1990]

recent = filter(simfile, year >= "1990-12-31")
recent = filter(recent, year <= "2006-12-31")
oil_change = colnames(recent)[5]
#mean(recent$`Arithmetic Return - Crude Oil`)
typeof(recent$`Arithmetic Return - Crude Oil`)
recent$`Arithmetic Return - Crude Oil`=  as.numeric(recent$`Arithmetic Return - Crude Oil`)
recent$`Arithmetic Return - Natural Gas`=  as.numeric(recent$`Arithmetic Return - Natural Gas`)
recent$`Arithmetic Return - Dry Well`=  as.numeric(recent$`Arithmetic Return - Dry Well`)

#sd(recent$`Arithmetic Return - Crude Oil`)

initial_cost = rowMeans(recent[16,2:4])
#recent = filter(recent, year >= "1990-12-31")
#dens2 <- density(row_avg, bw="SJ-ste")

#All combined in one vector of 48 observations
combined = c(recent$`Arithmetic Return - Crude Oil`,recent$`Arithmetic Return - Natural Gas`, recent$`Arithmetic Return - Dry Well`)

sd(combined)
dens <- density(combined, bw="SJ-ste")

tri_dens <- density(combined, kernel = triangular)

hist(combined, breaks = 10)

shapiro.test(combined)

ks.test(combined)

ad.test(combined)

qqplot(combined)

sim_2012_15 <- rtriangle(3, -.22, -.07, -.0917)

sim_2015_18 <- rtriangle(3,.02, .06, .05)

hist(sim_2015_18, breaks = 10)

Est_dens <- rkde(fhat=kde(combined, h=0.17411), n=1000)
hist(Est_dens, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')


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
#print(cost2019)
summary(cost2019)
hist(cost2019, breaks=100, main='2019 cost estimating', xlab='Average Cost')

cost_sim_1=as.data.frame(cost2019)
ggplot(cost_sim_1,aes(cost_sim_1$cost2019))+
  geom_histogram(breaks=seq(0,20000,by=100),fill='skyblue2')+ 
  ggtitle("2019 Drilling Cost Histogram from Simulation")+   
  xlab("Cost in dollar")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))

ntimes = 1000000
#ntimes = 100
cost2019_norm = rep(NA, ntimes)
for(j in 1:ntimes){
  set.seed(j)
  Est.x4 <-rnorm(n=6, mean=mean(combined), sd=sd(combined))
  Est.x5<-rtriangle(3, -0.22 , -0.07, -0.0917)
  Est.x6<-rtriangle(4, 0.02 , 0.06, 0.05)
  x7=c(Est.x4,Est.x5,Est.x6,recursive=TRUE)
  cost2019_norm[j]<-initial_cost*(prod(1+x7))
}
summary(cost2019_norm)
hist(cost2019_norm, breaks=100, main='2019 cost estimating', xlab='Average Cost')
sd(cost2019_norm)
cost_sim_1_norm=as.data.frame(cost2019_norm)
ggplot(cost_sim_1_norm,aes(cost_sim_1_norm$cost2019))+
  geom_histogram(breaks=seq(0,20000,by=100),fill='skyblue2')+ 
  ggtitle("2019 Drilling Cost Distribution")+   
  xlab("Cost ($)")+ylab("Frequency")+ 
  theme_bw()+ theme(plot.title = element_text(hjust = 0.5,size=22),         
  axis.title=element_text(size=18), axis.text = element_text(size=18,lineheight = 5))


leased_acres = rnorm(n=12, mean=600, sd=50)
acre_cost = 960
seismic_sections = rnorm(n=12, mean=3, sd=.35)
seismic_cost = 43000
completion_cost = rnorm(n=12, mean=390000, sd=50000)
overhead = rtriangle(1, 172000, 279500, 215000)

#need to add hw1 total to this
initial_exp = leased_acres*acre_cost+seismic_sections*seismic_cost+completion_cost+overhead

m = 420
s = 120
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))
#add correlation between these of 0.64

ip = rlnorm(10000, meanlog = location, sdlog = shape)
#ip = rlnorm(1000, meanlog = 420,sdlog = 120)
mean(ip)
sd(ip)
hist(ip)
decline_rate = runif(12, min = 0.15, max = 0.32)
final_year_rate = (1-decline_rate)*ip
oil_volume = (365*final_year_rate*ip)/2

oil_prices = read_excel('Analysis_Data.xlsx', sheet = 1, skip = 2)
#calculate triangle dist from df in line above for each year

nri = rnorm(1, mean = .75, sd = .2)

oil_rev = oil_volume*oil_prices*nri
tax = 0.06
final_rev = oil_rev*(1-tax)

op_exp = nri = rnorm(12, mean = 2.25, sd = .3)


