# read in the data from Korean, Italy and USA
library(dplyr)
setwd("~/Desktop/covid19-in-usa/covid_19_projection_03_16/code")
graphics.off()
k_data = read.csv('../data/K_time.csv')
k_daily = k_data %>%
  group_by(date) %>%
  summarise(test_daily = sum(test), pos_daily = sum(confirmed),
            rel_daily = sum(released), dec_daily = sum(deceased))
k_daily$date = as.Date(k_daily$date, "%Y-%m-%d")

quartz()
par(mfrow=c(2,2))
plot(test_daily ~ date, k_daily, xaxt = "n", type = "l",xlim= c(min(k_daily$date), min(k_daily$date) + 60))
axis(1, k_daily$date, format(k_daily$date, "%b %d"), cex.axis = .7)

plot(pos_daily ~ date, k_daily, xaxt = "n", type = "l",xlim= c(min(k_daily$date), min(k_daily$date) + 60))
axis(1, k_daily$date, format(k_daily$date, "%b %d"), cex.axis = .7)


plot(rel_daily ~ date, k_daily, xaxt = "n", type = "l",xlim= c(min(k_daily$date), min(k_daily$date) + 60))
axis(1, k_daily$date, format(k_daily$date, "%b %d"), cex.axis = .7)


plot(dec_daily ~ date, k_daily, xaxt = "n", type = "l", xlim= c(min(k_daily$date), min(k_daily$date) + 60))
axis(1, k_daily$date, format(k_daily$date, "%b %d"), cex.axis = .7)
title("Korea: Time Series(total tested, positive case, released, deceased) 
      from 2020-01-20 to 2020-03-12", line = -23, outer = TRUE)


# gathere italy data
i_data = read.csv('../data/covid19_italy_region.csv')
i_data$date = i_data$Date
i_daily = i_data %>%
  group_by(date) %>%
  summarise(test_daily = sum(TestsPerformed), pos_daily = sum(TotalPositiveCases),
            rel_daily = sum(Recovered), dec_daily = sum(Deaths))

i_daily$date = as.Date(i_daily$date, "%Y-%m-%d %H:%M:%S")

quartz()
par(mfrow=c(2,2))
plot(test_daily ~ date, i_daily, xaxt = "n", type = "l", xlim= c(min(i_daily$date), min(i_daily$date) + 60))
axis(1, i_daily$date, format(i_daily$date, "%b %d"), cex.axis = .7, col=2)

plot(pos_daily ~ date, i_daily, xaxt = "n", type = "l",xlim= c(min(i_daily$date), min(i_daily$date) + 60))
axis(1, i_daily$date, format(i_daily$date, "%b %d"), cex.axis = .7, col=2)


plot(rel_daily ~ date, i_daily, xaxt = "n", type = "l",xlim= c(min(i_daily$date), min(i_daily$date) + 60))
axis(1, i_daily$date, format(i_daily$date, "%b %d"), cex.axis = .7, col=2)

plot(dec_daily ~ date, i_daily, xaxt = "n", type = "l",xlim= c(min(i_daily$date), min(i_daily$date) + 60))
axis(1, i_daily$date, format(i_daily$date, "%b %d"), cex.axis = .7, col=2)
title("Italy: Time Series(total tested, positive case, released, deceased) 
      from 2020-02-24 to 2020-03-16", line = -23, outer = TRUE)

# gathere Us data
u_data = read.csv('../data/us_covid19_daily.csv')
u_data$tested = u_data$positive + u_data$negative
u_daily = u_data %>%
  group_by(date) %>%
  summarise(test_daily = sum(tested, na.rm = TRUE), pos_daily = sum(positive, na.rm = TRUE),
            rel_daily = 0, dec_daily = sum(death, na.rm = TRUE))

u_daily$date = as.Date(as.character(u_daily$date), "%Y%m%d") - 1
quartz()
par(mfrow=c(2,2))
plot(test_daily ~ date, u_daily, xaxt = "n", type = "l",xlim= c(min(u_daily$date), min(u_daily$date) + 60))
axis(1, u_daily$date, format(u_daily$date, "%b %d"), cex.axis = .7, col=3)


plot(pos_daily ~ date, u_daily, xaxt = "n", type = "l",xlim= c(min(u_daily$date), min(u_daily$date) + 60))
axis(1, u_daily$date, format(u_daily$date, "%b %d"), cex.axis = .7, col=3)


plot(rel_daily ~ date, u_daily, xaxt = "n", type = "l",xlim= c(min(u_daily$date), min(u_daily$date) + 60))
axis(1, u_daily$date, format(u_daily$date, "%b %d"), cex.axis = .7, col=3)


plot(dec_daily ~ date, u_daily, xaxt = "n", type = "l",xlim= c(min(u_daily$date), min(u_daily$date) + 60))
axis(1, u_daily$date, format(u_daily$date, "%b %d"), cex.axis = .7, col=3)

title("USA: Time Series(total tested, positive case, released, deceased) 
      from 2020-03-03 to 2020-03-15", line = -23, outer = TRUE)


run_division = function(x){
  return(unlist(sapply(2:length(x), function(i) x[i]/x[i-1])))}

x1 = run_division(k_daily$pos_daily)
x2 = run_division(i_daily$pos_daily)
x3 = run_division(u_daily$pos_daily)

quartz()
par(mfrow=c(2,2))
plot(x1, xlim = c(0, 60), ylim = c(1,2.2), xaxt="n", type = 'l', xlab ='Date', ylab = 'Daily change ratio' )
axis(side = 1, at = seq(1, length(x1)), labels = k_daily$date[2:length(k_daily$date)])


plot(x2, xlim = c(0, 60), ylim = c(1,2.2), xaxt="n", type = 'l', xlab ='Date', ylab = 'Daily change ratio' )
axis(side = 1, at = seq(1, length(x2)), labels = i_daily$date[2:length(i_daily$date)])


plot(x3, xlim = c(0, 60), ylim = c(1,2.2), xaxt="n", type = 'l', xlab ='Date', ylab = 'Daily change ratio' )
axis(side = 1, at = seq(1, length(x3)), labels = u_daily$date[2:length(u_daily$date)])

quartz()
ccf(x1, x3, lag.max = 10, plot = TRUE)
quartz()
ccf(x1[31:length(x1)], x3, lag.max = 10, plot = TRUE)
quartz()
ccf(x2, x3, lag.max = 5, plot = TRUE)


# Run US positive case from Italy 
projected = u_daily$pos_daily[length(u_daily$pos_daily)] * cumprod(x2[(length(u_daily$pos_daily)):length(x2)])
i_project = c(u_daily$pos_daily, projected)
write.csv(i_project, file = 'i.csv')

quartz()
plot(i_project,xlim = c(0, 30), xaxt="n", type = 'l', xlab ='Date', ylab = 'Daily change ratio' )
axis(side = 1, at = seq(1, length(i_project)), labels = i_daily$date+8)


x1_temp = x1[31:length(x1)]
projected = u_daily$pos_daily[length(u_daily$pos_daily)] * cumprod(x1_temp[(length(u_daily$pos_daily)):length(x1_temp)])
k_project = c(u_daily$pos_daily, projected)
write.csv(k_project, file = 'k.csv')

quartz()
plot(i_project,xlim = c(0, 30), xaxt="n", type = 'l', xlab ='Date', ylab = 'Daily change ratio' )
axis(side = 1, at = seq(1, length(i_project)), labels = i_daily$date+8)


# add in pure exponential increase 
# log(case) = a + b*t
us_time_case = data.frame(log(u_daily$pos_daily), seq(1, length(u_daily$pos_daily)))
colnames(us_time_case) = c('log_case', 'days')
t_frct = data.frame(days=seq(length(u_daily$pos_daily)+1, length(u_daily$pos_daily)+10))
crazy_model = lm(log_case~days, data = us_time_case)
write.csv(c(u_daily$pos_daily, exp(predict(crazy_model, t_frct))), file = 'crazy.csv')
