rm(list = ls())

 library(tidyverse)
 library(modelsummary)
 
 data <- read.csv("GSSdata2018.csv")
 head(data)
 dim(data)
 
 data <- data %>% filter(WRKSTAT == 1 | WRKSTAT == 2)
 dim(data)
 
 data <- data %>% mutate(income =
  500*(RINCOME == 1) + 
	1000*(RINCOME == 2) + 
	3000*(RINCOME == 3) + 
	4000*(RINCOME == 4) + 
	5000*(RINCOME == 5) + 
	6000*(RINCOME == 6) + 
	7000*(RINCOME == 7) + 
	8000*(RINCOME == 8) + 
	10000*(RINCOME == 9) + 
	15000*(RINCOME == 10) +
	20000*(RINCOME == 11) +  
	25000*(RINCOME == 12) ) %>% mutate(lnincome = log(income + 1))
 data <- data %>% mutate(belief = 1*(HELL == 1)*(HEAVEN == 1)) 
  
 mdata <- data %>% filter(SEX == 1)
 fdata <- data %>% filter(SEX == 2)
 
 models <- list()
 models[["male_level"]] <- lm(income ~ AGE + EDUC + belief, mdata)
 models[["male_log"]]   <- lm(lnincome ~ AGE + EDUC + belief, mdata)
 models[["female_level"]] <- lm(income ~ AGE + EDUC + belief, fdata)
 models[["female_log"]]   <- lm(lnincome ~ AGE + EDUC + belief, fdata)
 
 modelsummary(models,
	gof_omit = "Log.Lik.|AIC|BIC|F|RMSE",
	stars = TRUE,
	notes = "Standard errors in parentheses.")




 