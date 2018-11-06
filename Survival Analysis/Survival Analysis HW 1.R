library(data.table)
library(survival)
library(survminer)
library(muhaz)

katrina <- fread('C:\\Users\\gavin\\Desktop\\katrina.csv')

### Descriptive Statistics ###

#Read in data and create a seperate dataset grouped by reason called grouped_katrina
katrina <- read_csv('katrina.csv')

grouped_katrina <- group_by(katrina, reason)

#produce summary statistics for each failure reason
#Total num survive and total percent survived are statistics for the 
#entire dataset and not each reason

pump_summary <- summarise(grouped_katrina, 
          percent_of_pumps = n()/nrow(katrina),
          median_survival_time = median(hour),
          total_num_in_cat = n(), 
          median_age = median(age),
          median_slope = median(slope),
          slope_IQR = IQR(slope),
          total_num_survive = length(which(katrina["survive"]==1)),
          total_percent_survived = total_num_survive/nrow(katrina))
          

ggplot(katrina) +
    geom_histogram(mapping = aes(x=age), binwidth = .2)

ggplot(katrina) +
  geom_histogram(mapping = aes(x=slope),binwidth = 1)

ggplot(katrina) +
  geom_histogram(mapping = aes(x=elevation),binwidth = 1)

stats <- ggplot(katrina,  aes(as.factor(reason), slope)) +
  geom_boxplot()

stats_2 <- ggplot(katrina,  aes(as.factor(reason), age)) +
  geom_boxplot()


stats_3 <- ggplot(katrina,  aes(as.factor(reason), hour)) +
  geom_boxplot()

stats
stats_2
stats_3

quantile(katrina$slope)      

with(katrina, Surv(time = hour, event = survive == 0))

### survival curve ###
katrina_fit <- survfit(Surv(hour, survive == 0) ~ 1, data = katrina)
katrina_fit
summary(katrina_fit)

### survival curve plot ###
ggsurvplot(katrina_fit, data = katrina, conf.int = FALSE, palette = "grey")

### group by reason and plot each curve ###
katrina_reason <- survfit(Surv(time = hour, event = survive == 0) ~ reason, data = katrina)
ggsurvplot(katrina_reason, conf.int = FALSE)

### log-rank test ###
survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = katrina, subset = reason != 0)
pairwise_survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason != 0,])

### hazard functions ###
katrina$hour49 <- ifelse(katrina$hour == 48 & katrina$survive == 1, 49, katrina$hour)
katrina$failure <- ifelse(katrina$survive == '1', 0,1)

# kphaz.fit() has the same arguments as Surv()
katrina_haz <- with(katrina, kphaz.fit(hour49, failure))
# and we plot it with kphaz.plot()
kphaz.plot(katrina_haz, main="Hazard Function")

### cumulative hazard functions ###
ggsurvplot(katrina_fit, fun = "cumhaz", palette = "grey")
