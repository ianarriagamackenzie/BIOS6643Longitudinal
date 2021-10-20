# Homework 3
# Ian Arriaga Mackenzie

# libraries
library(tidyverse)
library(MASS)
library(msm)
library(sandwich)
library(lme4)

# set seed
set.seed(1)

# load data
cer_df = read.csv("~/GitHub/BIOS6643Longitudinal/homework3/Cereal2.csv")

# filter family member 3
kid1_df = cer_df %>% 
  filter(FamMem == 3) %>% 
  dplyr::select(Cond, C1, Sex, Wt1, FamIDNO) %>% 
  mutate(C1_rne = C1*exp(rnorm(99, 0, 1)))


# poisson fit
p_fit = glm(formula = C1 ~ Cond + Sex + Wt1,
            family = "poisson",
            data = kid1_df)
summary(p_fit)
exp(coef(p_fit))
exp(confint(p_fit))

# quasipoisson fit
qp_fit = glm(formula = C1 ~ Cond + Sex + Wt1,
             family = "quasipoisson",
             data = kid1_df)
summary(qp_fit)
exp(coef(qp_fit))
exp(confint(qp_fit))

# poisson with random normal error
rne_fit = glmer(formula = C1 ~ Cond + Sex + Wt1 + (1|FamIDNO),
                family = "poisson",
                data = kid1_df)
summary(rne_fit)
exp(coef(rne_fit))
exp(confint(rne_fit))

# NB fit
nb_fit = glm.nb(formula = C1 ~ Cond + Sex + Wt1,
                data = kid1_df)
summary(nb_fit)
