library(readxl)
library(lubridate)
library(dplyr)
library(ks)
library(triangle)
library(nortest)
library(ggplot2)

simfile = read_excel('C:\\Users\\gavin\\Documents\\GitHub\\Orange-HW-Team9\\Orange-HW-Team9\\Simulation and Risk\\Analysis_Data.xlsx', sheet = 2, skip = 2)
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


standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

#######################################################################################
################################# Starting Project LOOP ###############################
#######################################################################################
loop = 100000
final_wet_well = rep(NA, loop)
final_dry_well = rep(NA, loop)
for (l in 1:loop){
  wells = runif(1, min = 10, max = 30)
  hydrocarbon = rtruncnorm(wells, a=0, b=1, mean = .99, sd = .05)
  reservoir = rtruncnorm(wells, a=0, b=1, mean = .8, sd = .1)
  prob_wet = hydrocarbon*reservoir
  bernoulli_dist = rbinom(wells, size = 1, prob = prob_wet)
  wet_wells = sum(bernoulli_dist)
  dry_wells = wells - floor(wet_wells)
  sample_prob =  wet_wells/(floor(wells))
  

leased_acres = rnorm(n=1, mean=600, sd=50)
acre_cost = 960
seismic_sections = rnorm(n=1, mean=3, sd=.35)
seismic_cost = 43000
completion_cost = rnorm(n=1, mean=390000, sd=50000)
overhead = rtriangle(1, 172000, 279500, 215000)

#need to add hw1 total to this
initial_exp = leased_acres*acre_cost+seismic_sections*seismic_cost+completion_cost+overhead

 times = dry_wells
 total_exp = rep(NA,times)
 for(j in 1:times){
   Est.Leased <- rnorm(1, 600, 50) * acre_cost
   Est.Seismic <-rnorm(1, 3, .35) * seismic_cost
   Est.Overhead <-rtriangle(1, 172000, 279500, 215000)
   intital_exp <- Est.Overhead + Est.Seismic + Est.Leased
  
   Est.x4 <-rnorm(n=6, mean=mean(combined), sd=sd(combined))
   Est.x5<-rtriangle(3, -0.22 , -0.07, -0.0917)
   Est.x6<-rtriangle(4, 0.02 , 0.06, 0.05)
   x7=c(Est.x4,Est.x5,Est.x6,recursive=TRUE)
   cost2019_norm <-initial_cost*(prod(1+x7))
  
   total_exp[j] <- initial_exp + (cost2019_norm * 1000)
 }
 final_dry_well[l] <- sum(total_exp)
 
 
 ntimes = wet_wells
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
   
   R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
   U <- t(chol(R))
   Both.r <- cbind(standardize(decline_rate), standardize(ip))
   SB.r <- U %*% t(Both.r)
   SB.r <- t(SB.r)
   final.SB.r <- cbind(destandardize(SB.r[,2], decline_rate) ,destandardize(SB.r[,1], ip))
   ip = final.SB.r[1,2]
   decline_rate = final.SB.r[1,1]
  
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
  
   
  
   #calculate oil prices
   oil_prices = read_excel('C:\\Users\\gavin\\Documents\\GitHub\\Orange-HW-Team9\\Orange-HW-Team9\\Simulation and Risk\\Analysis_Data.xlsx', sheet = 1, skip = 2)
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
 final_wet_well[l] <- sum(done)
}

#######################################################################################
################################# End of Project LOOP #################################
#######################################################################################

### Summary stats For Final Output ###
all_wells = final_wet_well-final_dry_well
summary(final_dry_well)

summary(all_wells)
hist(all_wells)

#### Creating Project NPV Distribution ####
VaR = quantile(all_wells, probs = .05, na.rm = TRUE)
VaR.Label = dollar(VaR)
all_wells.df <- as.data.frame(all_wells)
ES <- filter(all_wells.df, all_wells < VaR)

Expected <- mean(all_wells, na.rm = TRUE)
Exp.label <- dollar(Expected)
ES <- mean(ES$all_wells)
dollar(ES)

Exp.U <- quantile(all_wells, 0.75, na.rm=TRUE)
Exp.L <- quantile(all_wells, 0.25, na.rm=TRUE)

dollar(Exp.L)
dollar(Exp.U)


hist(all_wells/1000000, breaks=50, main='Project NPV Distribution', xlab='NPV (in Millions)', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, col="lightblue")
abline(v = Expected/1000000, col="red", lwd=2)
mtext(paste("Expected Value",Exp.label, sep=" = "), at = Expected/1000000, col="red", cex = 1.25)
abline(v = Exp.L/1000000, col="blue", lwd=2, lty="dashed")
abline(v = Exp.U/1000000, col="blue", lwd=2, lty="dashed")

#### Bootstraping Simulation ####

n.bootstraps <- 10000
sample.size <- 10000

VaR.boot <- rep(0,n.bootstraps)
ES.boot <- rep(0,n.bootstraps)
for(i in 1:n.bootstraps){
  bootstrap.sample <- sample(all_wells, size=sample.size)
  VaR.boot[i] <- quantile(bootstrap.sample, .05, na.rm=TRUE)
  ES.boot[i] <- mean(bootstrap.sample[bootstrap.sample < VaR.boot[i]], na.rm=TRUE)
}


############ Value at Risk Plot #############

summary(VaR.boot)

VaR.boot.U <- quantile(VaR.boot, 0.975, na.rm=TRUE)
VaR.boot.L <- quantile(VaR.boot, 0.025, na.rm=TRUE)

dollar(VaR.boot.L)
dollar(VaR.boot.U)

hist(all_wells/1000000, breaks=50, main='Project NPV Distribution with VaR', xlab='NPV (in Millions)',cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, col="lightblue")
#breaks = c(100, 200, 300, 400, 500)
#axis(1, at = breaks, labels = paste("$", breaks, "M", sep = ""))
abline(v = VaR/1000000, col="red", lwd=2)
mtext(paste("Value at Risk",VaR.Label, sep=" = "), at = VaR/1000000, col="red", cex = 1.25)
abline(v = VaR.boot.L/1000000, col="blue", lwd=2, lty="dashed")
abline(v = VaR.boot.U/1000000, col="blue", lwd=2, lty="dashed")

########### Estimated Shortfall Plot #############

summary(ES.boot)

ES.boot.U <- quantile(ES.boot, 0.975, na.rm=TRUE)
ES.boot.L <- quantile(ES.boot, 0.025, na.rm=TRUE)
dollar(ES.boot.L)
ES.label <- dollar(ES)
dollar(ES.boot.U)

hist(all_wells/1000000, breaks=50, main='Project NPV Distribution with ES', xlab='NPV (in Millions)',cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, col="lightblue")
#breaks = c(100, 200, 300, 400, 500)
#axis(1, at = breaks, labels = paste("$", breaks, "M", sep = ""))
abline(v = ES/1000000, col="red", lwd=2)
mtext(paste("Estimated Shortfall",ES.label, sep=" = "), at = ES/1000000, col="red", cex = 1.25)
abline(v = ES.boot.L/1000000, col="blue", lwd=2, lty="dashed")
abline(v = ES.boot.U/1000000, col="blue", lwd=2, lty="dashed")

