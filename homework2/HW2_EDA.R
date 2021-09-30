# Longitudinal
# HW2

library(tidyverse)

eno_df = read.csv("~/GitHub/BIOS6643Longitudinal/homework2/eno_data.txt", sep="")

prcomp(eno_df)

lm(eno_df, formula = eno_post ~ eno_pre)
