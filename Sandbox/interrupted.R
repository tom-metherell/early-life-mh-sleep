library(tidyverse)
library(magrittr)
library(mgcv)
library(segmented)

x <- rnorm(1000, mean = 7.5, sd = 1)
rand <- rnorm(1000, mean = 0, sd = 0.5)
simdata <- data.frame(x, rand)
simdata %<>% mutate(y = (0.1*(x-7.5)^2 + 19.5 + rand))

gam_ <- gam(y ~ s(x, bs = "cr", k = 10), data = simdata)
spl_fit <- predict.gam(gam_, se.fit = TRUE)
yhat <- spl_fit$fit
y.se <- spl_fit$se.fit
simdata <- data.frame(simdata, yhat, y.se)
ymin <- min(simdata$yhat)
xmin <- simdata[simdata$yhat == min(simdata$yhat),]$x
y.lb <- simdata$yhat - simdata$y.se
simdata %<>% mutate(flat = y.lb < ymin)
flat_med <- median(simdata[simdata$flat == TRUE,]$x)

interim <- segmented(lm(y ~ x, data = simdata), psi = flat_med, control = seg.control(it.max = 0))
int.df1 <- data.frame(x = c(0, interim$psi[2]), y = c(interim$coefficients[1], interim$coefficients[1] + interim$psi[2]*interim$coefficients[2]))
int.df2 <- data.frame(x = c(interim$psi[2], 12), y = c(interim$coefficients[1] + interim$psi[2]*interim$coefficients[2], interim$coefficients[1] + interim$psi[2]*interim$coefficients[2] + (12-interim$psi[2])*(interim$coefficients[2] + interim$coefficients[3])))
coefs <- summary(interim, var.diff = TRUE)
t1 <- abs(coefs[["coefficients"]][[8]])
t2 <- abs(coefs[["coefficients"]][[9]])
percentile <- t2/(t1+t2)
flat_perc <- as.numeric(quantile(simdata[simdata$flat == TRUE,]$x, percentile))

model <- segmented(lm(y ~ x, data = simdata), psi = flat_perc, control = seg.control(it.max = 0))
df1 <- data.frame(x = c(0, model$psi[2]), y = c(model$coefficients[1], model$coefficients[1] + model$psi[2]*model$coefficients[2]))
df2 <- data.frame(x = c(model$psi[2], 12), y = c(model$coefficients[1] + model$psi[2]*model$coefficients[2], model$coefficients[1] + model$psi[2]*model$coefficients[2] + (12-model$psi[2])*(model$coefficients[2] + model$coefficients[3])))

ggplot(mapping = aes(x = x)) +
  geom_point(data = simdata, mapping = aes(y = y), size = 0.1, colour = "grey") +
  geom_point(data = simdata, mapping = aes(y = yhat, colour = flat)) +
  geom_path(data = df1, mapping = aes(y = y), colour = "blue") +
  geom_path(data = df2, mapping = aes(y = y), colour = "blue") +
  theme_classic()