# Homework 3
# Ian Arriaga Mackenzie

# libraries
library(tidyverse)
library(MASS)

# load data
cer_df = read.csv("~/GitHub/BIOS6643Longitudinal/homework3/Cereal2.csv")

# filter family member 3
kid1_df = cer_df %>% 
  filter(FamMem == 3) %>% 
  dplyr::select(Cond, C1, Sex, Wt1)

# poisson fit
p_fit = glm(formula = Cond ~ C1 + Sex + Wt1,
            family = "poisson",
            data = kid1_df)
summary(p_fit)

# quasipoisson fit
qp_fit = glm(formula = Cond ~ C1 + Sex + Wt1,
            family = "quasipoisson",
            data = kid1_df)
summary(qp_fit)

# NB fit
nb_fit = glm.nb(formula = Cond ~ C1 + Sex + Wt1,
                data = kid1_df)
summary(nb_fit)
