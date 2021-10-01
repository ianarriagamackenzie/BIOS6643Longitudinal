# Longitudinal
# HW2

# load libraries
library(tidyverse)
library(ggfortify)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(emmeans)



#### Q1

# load eNO data
eno_df = read.csv("~/GitHub/BIOS6643Longitudinal/homework2/eno_data.txt", sep="") %>% 
  rename(pre = eno_pre,
         post = eno_post)

# set colorblind colors
colpallet = brewer.pal(8, "Dark2")
colval = c("LM" = colpallet[1], "PCA" = colpallet[2])

# PCA
eno_pca = prcomp(eno_df)

# pca slope
pca_slope = eno_pca$rotation[2,1]/eno_pca$rotation[1,1]
# eno pre/post mean
mp = c(mean(eno_df$pre), mean(eno_df$post))
# y intercept using pca slope and passing through mean
yint = pca_slope * (0 - mp[1]) + mp[2]

# Plot LM and PCA slope over data
ggplot(eno_df, aes(pre, post))+
  geom_point() +
  geom_smooth(aes(color = "LM"),
              method = "lm", 
              formula = y ~ x,
              se = F,
              size = 1) +
  geom_abline(color = colval[2],
              slope = pca_slope,
              intercept = yint,
              size = 1) +
  theme_bw() +
  labs(x = "Pre",
       y = "Post") +
  scale_colour_manual(name = element_blank(),
                      values = colval, 
                      guide = guide_legend(override.aes = aes(fill = NA)))
  
#### Q2

bc_df = read.csv("~/GitHub/BIOS6643Longitudinal/homework2/beta_carotene_univar.csv")

bc_cl = bc_df %>% 
  rename(group = Prepar,
         id = Id,
         carotene = y,
         time = time) %>% 
  mutate(group = factor(group,
                        levels = c(1,2,3,4),
                        labels = c("Solatene_30mg",
                                   "Roche_60mg",
                                   "BASF_30mg",
                                   "BASF_60mg")),
         time = factor(time,
                       levels = c(0,6,8,10,12),
                       labels = c("0_m", "6_m", "8_m", "10_m", "12_m")))



bc_fit = lmer(formula = carotene ~ time + group + group*time + (1 | id) - 1,
              data = bc_cl)
summary(bc_fit)
anova(bc_fit)


bc_em = emmeans(bc_fit, c("time", "group"))

bc_em

contrast(bc_em, "tukey")

contrast(bc_em)



bc_em = emmeans(bc_fit, "time", by = "group")

bc_em

contrast(bc_em, "tukey")

contrast(bc_em)

