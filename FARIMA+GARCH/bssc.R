if (!require("pacman")) install.packages("pacman")

pacman::p_load(lattice, bursts, denstrip, rstan, brms, ggplot2, rstanarm,
               bayesplot, bursts, forecast)

library("pacman")
setwd("I:/dropbox/Dropbox/Shouhuai-related/Zheyuan-Research/data")

eventdata <- read.csv(file = 'cyber_event_binned_data.csv',
                      stringsAsFactors = F)
binnedCount <- eventdata$event.count
binnedLen <- eventdata$avg.report.length
weekvar <- eventdata$week
count.data <- cbind(weekvar, binnedCount, binnedLen)
colnames(count.data) <- c("Time", "Counts", "Length")
count.data <- data.frame(count.data)
t <- weekvar

set.seed(9999)
fit1 <- brm(Counts ~ Time, data = count.data, autocor = cor_bsts(formula = ~1),
            family = negbinomial, iter = 12000, control = list(max_treedepth = 15))

fit2 <- brm(Counts ~ Time + Length, data = count.data, autocor = cor_bsts(formula = ~1),
            family = negbinomial, iter = 2000, control = list(max_treedepth = 15))

brm.fit.btstp <- data.frame(predict(fit1, probs = c(0.025, 0.975)))
est <- brm.fit.btstp[,1]

MAE <- sum(abs(count.data$Counts-est))/length(count.data$Counts)
MAE

sMAPE <- 1/length(count.data$Count) * sum(abs(count.data$Count- est)/(count.data$Count + est))
sMAPE