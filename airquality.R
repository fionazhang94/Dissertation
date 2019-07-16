#### Packages used
library(visreg)
library(tidyverse)
library(magrittr)
library(bootstrap)

### multivariable 

# Prepare Data
air <- as_tibble(airquality[, 1:4]) %>% 
  na.omit()

# Base model
fit2 <- lm(Ozone ~., data = air)

# base model result
a <- visreg(fit2, plot = F,nn=111)
# check the help page of visreg function. Here is a list of length 3, corresponds to Solar.R, Wind, and Temp variable

# assign names for later usage
names_a <- a %>% map(~ .$meta$x) %>% unlist()
names(a) <- names_a

# fitted values
a1 <- a %>% 
  map(~ .$fit) %>% 
  map(as_tibble)

dat1_base <- tibble()
for (i in seq_along(names_a)) {
  tmp <- a1[[names_a[i]]] %>% 
    select(value = names_a[i], Ozone = visregFit) %>% 
    mutate(variable = names_a[i])
  dat1_base <- bind_rows(dat1_base, tmp)
}

# partial residuals
a2 <- a %>% 
  map(~ .$res) %>% 
  map(as_tibble)

dat2_base <- tibble()
for (i in seq_along(names_a)) {
  tmp <- a2[[names_a[i]]] %>% 
    select(value = names_a[i], Ozone = visregRes) %>% 
    mutate(variable = names_a[i])
  dat2_base <- bind_rows(dat2_base, tmp)
}

# partial residual plot as base plot, 
# and add bootstraped fitted lines to it.
gg_base <- dat2_base %>%
  ggplot(aes(x = value, y = Ozone)) +
  geom_point() +
  facet_wrap(~variable, scales = "free")

gg_base

#################### pair bootstrap step ########################

# function used in bootstrap 
f2 <- function(selected){
  dat <- air[selected, ]
  lm(Ozone ~ ., data = dat)
  # returns a lm object
}


# boostrap
n_draws <- 100
bootvals_2 <- bootstrap(1:nrow(air), n_draws, f2)

# gather lines

# f3 takes a lm object and return the partial residuals
f3 <- function(fit) {
  dat <- fit$model
  
  a <- visreg(fit, plot = F)
  names_a <- a %>% map(~ .$meta$x) %>% unlist()
  names(a) <- names_a
  
  a1 <- a %>% 
    map(~ .$fit) %>% 
    map(as_tibble)
  
  # fitted values
  dat1 <- tibble()
  for (i in seq_along(names_a)) {
    tmp <- a1[[names_a[i]]] %>% 
      select(value = names_a[i], Ozone = visregFit) %>% 
      mutate(variable = names_a[i])
    dat1 <- bind_rows(dat1, tmp)
  }
  dat1
}

# iterate f3 over bootstrapped values to gather fitted lines: 
g <- gg_base
for (i in 1:n_draws) {
  fit <- bootvals_2$thetastar[[i]]
  dat1 <- f3(fit)
  g <- g +
    geom_line(aes(x = value, y = Ozone), data = dat1, alpha = 0.15, color = "grey60")
}

# may take a while to plot
g +
  # add fitted values for each variable
  geom_line(aes(x = value, y = Ozone), data = dat1_base, color = "blue", lwd = 1)

###################residual bootstrap#########################


theta.res  <- function(res,Solar.R,Wind,Temp,al,be1,be2,be3)
{
  # xdata contains the Solar.R
  y.boot  <- al+be1*Solar.R+be2*Wind+be3*Temp + res
  answer  <- lm(y.boot~Solar.R+Wind+Temp)
}
n_draws=100
reg <- lm(Ozone~.,data = air)
res  <- residuals(reg)
al <- coef(reg)[1]
be1 <- coef(reg)[2]
be2 <- coef(reg)[3]
be3 <- coef(reg)[4]
bootvals  <- bootstrap(res,n_draws,theta.res,Solar.R=air$Solar.R, Wind=air$Wind, Temp=air$Temp, al=al,be1=be1,be2=be2,be3=be3)
# f3 takes a lm object and return the partial residuals
f3 <- function(fit) {
  dat <- fit$model
  
  a <- visreg(fit, plot = F)
  names_a <- a %>% map(~ .$meta$x) %>% unlist()
  names(a) <- names_a
  
  a1 <- a %>% 
    map(~ .$fit) %>% 
    map(as_tibble)
  
  # fitted values
  dat1 <- tibble()
  for (i in seq_along(names_a)) {
    tmp <- a1[[names_a[i]]] %>% 
      select(value = names_a[i], Ozone = visregFit) %>% 
      mutate(variable = names_a[i])
    dat1 <- bind_rows(dat1, tmp)
  }
  dat1
}

# iterate f3 over bootstrapped values to gather fitted lines: 
g <- gg_base
for (i in 1:n_draws) {
  fit <- bootvals_2$thetastar[[i]]
  dat1 <- f3(fit)
  g <- g +
    geom_line(aes(x = value, y = Ozone), data = dat1, alpha = 0.15, color = "grey60")
}

# may take a while to plot
g +
  # add fitted values for each variable
  geom_line(aes(x = value, y = Ozone), data = dat1_base, color = "blue", lwd = 1)












