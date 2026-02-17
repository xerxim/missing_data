#main R von Tom (;

# Place for main analysis code.
# Imports:
## Libraries.
library(tidyverse)
library(mice)
## Self written code.
mc <- new.env()
sys.source("project/src/mc_study.R", envir = mc)
source("project/src/mice.impute.cart_boot.R")
# Working constants:
plot_path <- "project/plots/" # Use file.path(plot_path, "plotname") to safe.

# Analysis:
mc$mc_study()
# Test run.
t_30mX3 <- mc$mc_study(
  c("cart","cart_boot", "pmm"), m = 30, "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X3"=12.8), 
  n = 500, cycles =  200, c("X3"), 
  "MCAR", c(NULL, NULL, 0.3), NULL, NA
)


t_40mX3 <- mc$mc_study(
  c("cart","cart_boot", "pmm"), 30, "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X3"=12.8), 
  500, 200, c("X1", "X2", "X3"), 
  "MCAR", c(NULL, NULL, 0.4), NULL, NA
)

t_20mX3 <- mc$mc_study(
  c("cart","cart_boot", "pmm"), 30, "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X3"=12.8), 
  500, 200, c("X1", "X2", "X3"), 
  "MCAR", c(NULL, NULL, 0.2), NULL, NA
)

t_10mX3 <- mc$mc_study(
  c("cart","cart_boot", "pmm"), 30, "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X3"=12.8), 
  500, 200, c("X1", "X2", "X3"), 
  "MCAR", c(NULL, NULL, 0.1), NULL, NA
)

t_50mX3 <- mc$mc_study(
  c("cart","cart_boot", "pmm"), 30, "X3 ~ X1 +X2",
  c("(Intercept)"=5, "X1"=0.6, "X2"=0.5, "X3"=12.8), 
  500, 200, c("X1", "X2", "X3"), 
  "MCAR", c(NULL, NULL, 0.5), NULL, NA
)

t_10mX3_sum <- t_10mX3 %>%
  group_by(method, term) %>%
  summarise(coverage = mean(as.numeric(cover), na.rm = TRUE)) %>%
  mutate(missperc = 10)

t_20mX3_sum <- t_20mX3 %>%
  group_by(method, term) %>%
  summarise(coverage = mean(as.numeric(cover), na.rm = TRUE)) %>%
  mutate(missperc = 20)

t_30mX3_sum <- t_30mX3 %>%
  group_by(method, term) %>%
  summarise(coverage = mean(as.numeric(cover), na.rm = TRUE)) %>%
  mutate(missperc = 30)

t_40mX3_sum <- t_40mX3 %>%
  group_by(method, term) %>%
  summarise(coverage = mean(as.numeric(cover), na.rm = TRUE))%>%
  mutate(missperc = 40)

t_50mX3_sum <- t_50mX3 %>%
  group_by(method, term) %>%
  summarise(coverage = mean(as.numeric(cover), na.rm = TRUE))%>%
  mutate(missperc = 50)

### Coverage Plots

sum_sum <- rbind(
  t_10mX3_sum,
  t_20mX3_sum,
  t_30mX3_sum,
  t_40mX3_sum,
  t_50mX3_sum
)

sum_sum_b0 <- sum_sum[sum_sum$term == "(Intercept)",]

ggplot(sum_sum_b0)+
  geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4)+
  geom_point(aes(x = missperc, y = coverage, color = method))+
  geom_hline(aes(yintercept = 0.9), lty = 2)+
  ylim(c(0,1))+
  labs(title = "Beta 0")+
  theme_classic()


sum_sum_b1 <- sum_sum[sum_sum$term == "X1",]

ggplot(sum_sum_b1)+
  geom_line(aes(x = missperc, y = coverage, color = method), alpha = 0.4)+
  geom_point(aes(x = missperc, y = coverage, color = method))+
  geom_hline(aes(yintercept = 0.9), lty = 2)+
  ylim(c(0,1))+
  labs(title = "Beta 1")+
  theme_classic()

