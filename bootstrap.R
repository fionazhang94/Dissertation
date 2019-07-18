library('bootstrap')
library('ggplot2')
library(dplyr, warn.conflicts = FALSE)

theta.lr <- function(selected,xdata)
{
  dummy <- lm(xdata[selected,1]~xdata[selected,2])
  answer <- dummy$coef
  answer
}

# Prepare Data
airquality <- as_tibble(airquality[, 1:4]) %>% 
  na.omit()

ft1 <- lm(Ozone~Solar.R, data = airquality)
summary(ft1)

ggplot(airquality) + 
  aes(x = Solar.R, y = Ozone) + 
  # Plot the orginal linear regression in blue
  geom_abline(intercept = coef(ft1)[1], 
              slope = coef(ft1)[2], 
              size = 1, color = "#3366FF") +
  geom_point() + theme_classic()

######################## bootstrap pairs ###########################
n_draws=100
bootvals <- bootstrap(1:111,n_draws,theta.lr,xdata=cbind(airquality$Ozone,airquality$Solar.R))

fits <- data.frame(intercept=bootvals$thetastar[1,],slope=bootvals$thetastar[2,])
ggplot(airquality) + 
  aes(x = Solar.R, y = Ozone) + 
  # Plot a random sample of rows as gray semi-transparent lines
  geom_abline(aes(intercept = intercept, slope = slope), 
              data = sample_n(fits, n_draws), color = "grey60", 
              alpha = .15) + 
  # Plot the orginal linear regression in blue
  geom_abline(intercept = coef(ft1)[1], 
              slope = coef(ft1)[2], 
              size = 1, color = "#3366FF") +
  geom_point() + theme_classic()

######################## bootstrap residual #######################
theta.res  <- function(res,xdata,alpha,beta)
{
  # xdata contains the Solar.R
  y.boot  <- alpha + beta * xdata + res
  answer  <- lm(y.boot~xdata)
  answer <- answer$coef
  answer
}
n_draws=100
res.vals  <- residuals(lm(airquality$Ozone~airquality$Solar.R))
al        <- lm(airquality$Ozone~airquality$Solar.R)$coef[1]
be        <- lm(airquality$Ozone~airquality$Solar.R)$coef[2]
bootvals  <- bootstrap(res.vals,n_draws,theta.res,xdata=airquality$Solar.R,alpha=al,beta=be)
fits <- data.frame(intercept=bootvals$thetastar[1,],slope=bootvals$thetastar[2,])
ggplot(airquality) + 
  aes(x = Solar.R, y = Ozone) + 
  # Plot a random sample of rows as gray semi-transparent lines
  geom_abline(aes(intercept = intercept, slope = slope), 
              data = sample_n(fits, n_draws), color = "grey60", 
              alpha = .15) + 
  # Plot the orginal linear regression in blue
  geom_abline(intercept = coef(lm(Ozone~Solar.R, data = airquality))[1], 
              slope = coef(lm(Ozone~Solar.R, data = airquality))[2], 
              size = 1, color = "#3366FF") +
  geom_point() 












