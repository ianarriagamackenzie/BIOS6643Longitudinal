# Longitudinal Analysis HW 1
# Ian Arriaga MacKenzie

library(tidyverse)
library(ggfortify)
library(lme4)
library(astsa)
library(forecast)

# Q1

chol_raw = read.csv("~/GitHub/BIOS6643Longitudinal/homework1/cholesterol.csv")
chol_df = chol_raw %>% 
  mutate(diff = after - before,
         subid = as.numeric(row.names(chol_raw)))

# A
cs_lm = lm(diff ~ 1, chol_df)

summary(cs_lm)
confint(cs_lm)

# B
t.test(chol_df$diff)

# C
bac_lm = lm(after ~ before, chol_df)

summary(bac_lm)
confint(bac_lm)

# D
autoplot(cs_lm)
autoplot(bac_lm)

# E
hy_lm = lm(diff ~ before, chol_df)

summary(hy_lm)
confint(hy_lm)

# F

chol_lme = lmer(diff ~ before + (1|before), chol_df)
summary(chol_lme)


# Q3

gt_df = read.csv("~/GitHub/BIOS6643Longitudinal/homework1/global_temp_anomalies.csv")

gt_ts = ts(gt_df$temp, frequency = 1, start = 1880)

# A
ar1_fit = sarima(gt_ts, 1, 0, 0)
ar1_fit$fit

lm(temp ~ year,
   gt_df)

# auto_ar = auto.arima(gt_ts)
# summary(auto_ar)
# checkresiduals(auto_ar)

# B
v_t = as.numeric(resid(ar1_fit$fit))
v_t_up = c(0, v_t)

newres = v_t_up + 0.9484*v_t

plot(1:140, newres[2:141],
     xlab = "Time",
     ylab = "Residuals",
     main = "New Residuals")


# D
poly_fit = lm(formula = temp ~ poly(year, 4),
              data = gt_df)
autoplot(poly_fit)
summary(poly_fit)
AIC(poly_fit)
logLik(poly_fit)


# E
loess_fit = loess(formula = temp ~ year,
                  data = gt_df,
                  span = .30)
summary(loess_fit)

plot(residuals(loess_fit))
qqnorm(residuals(loess_fit))

plot(loess_fit)
