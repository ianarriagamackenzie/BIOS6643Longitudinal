# Longitudinal Analysis HW 1
# Ian Arriaga MacKenzie

library(tidyverse)

chol_raw = read.csv("C:/Users/iansa/OneDrive/Desktop/School/LongitudinalAnalysis/cholesterol.csv")

chol_df = chol_raw %>% 
  mutate(diff = after - before)

cs_lm = lm(diff ~ 1, chol_df)

summary(cs_lm)
mean(chol_df$diff)
t.test(chol_df$diff)

bac_lm = lm(after ~ before, chol_df)

summary(bac_lm)


gt_df = read.csv("C:/Users/iansa/OneDrive/Desktop/School/LongitudinalAnalysis/global_temp_anomalies.csv")

ar(gt_df$temp)

test = ts.union(gt_df)
test_ar = ar(test)

ar(gt_df$temp)

test_ar$resid