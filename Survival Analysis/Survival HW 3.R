library(dplyr)
library(tidyverse)
library(zoo)

pump<- read.csv("katrina.csv", header = TRUE)
pump$ID <- 1:nrow(pump)
pump_gather <- pump %>%
  gather(hours, pumping, h1:h48)

pump_gather = arrange(pump_gather, ID)

pump_gather$stop = substring(pump_gather$hours, 2)
pump_gather$stop = as.numeric(pump_gather$stop)
pump_gather$start = pump_gather$stop - 1
miss <- is.na(pump_gather$pumping)
pump_gather$pumping[miss] <- 0
pump_gather$cum_sum <- ave(pump_gather$pumping, pump_gather$ID, cumsum(pump_gather$pumping ==0 ), FUN=cumsum)
pump_gather$rolling <- as.numeric(pump_gather$cum_sum>12)
View(pump_gather)


change_rows = which(pump_gather$rolling==0 & dplyr::lag(pump_gather$rolling)==1 & pump_gather$hours != 'h1')
pump_gather$rolling[change_rows] = 1

rows = which(pump_gather$hour < pump_gather$stop)

pump_gather$pumping[rows] = 0
sum(is.na(pump_gather$pumping))

View(pump_long)
pump_long = ungroup(pump_long)
pump_long <-pump_long %>%
  mutate(failed= 
           ifelse(((pump_long$reason==2) & (pump_long$stop==pump_long$hour)),1,0))


fit_12 <- coxph(Surv(start, stop, event = failed)~backup+ bridgecrane+ servo
                + elevation+ slope+ age+ rolling, data = pump_long)
summary(fit_12)