library(tidyverse)
library(modelsummary)
library(readr)
library(ggplot2)

data <- read_csv("GSSdata2018.csv")
View(data)

head(data)
dim(data)

summary(data)

# Data description


# Data visulization

# Age distribution
ggplot(data, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Education distribution
ggplot(data, aes(x = EDUC)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Education Distribution in Years", x = "Years of Education", y = "Density")

# Gender distribution
ggplot(data, aes(x = factor(SEX), fill = factor(SEX))) +
  geom_bar() +
  scale_fill_manual(values = c("1" = "lightblue", "2" = "pink")) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female"))

# Respondent Income distribution
ggplot(data, aes(x = RINCOME)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Income Distribution", x = "Income", y = "Frequency")

# Family Income distribution
ggplot(data, aes(x = INCOME)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Income Distribution", x = "Income", y = "Frequency")

# Health distribution
ggplot(data, aes(x = HEALTH)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Health Distribution", x = "Health", y = "Frequency")

# Happyness distribution
ggplot(data, aes(x = HAPPY)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Happyness Distribution", x = "Happyness", y = "Frequency")



# Econometric models (Regression models)
model_edu_sex <- lm(EDUC ~ SEX, data)

summary(model_edu_sex)
modelsummary(model_edu_sex,
             stars = TRUE,
             notes = "Standard errors in parentheses.")

ggplot(data, aes(x = SEX, y = EDUC)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Plot of EDUC ~ SEX",
       x = "Sex",
       y = "Education")


model_income <- lm(RINCOME ~ INCOME, data)

summary(model_income)
modelsummary(model_income,
             stars = TRUE,
             notes = "Standard errors in parentheses.")

ggplot(data, aes(x = INCOME, y = RINCOME)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Plot of RINCOME ~ INCOME",
       x = "INCOME",
       y = "RINCOME")

model_edu <- lm(EDUC ~ MAEDUC + PAEDUC, data)

summary(model_edu)
modelsummary(model_edu,
             stars = TRUE,
             notes = "Standard errors in parentheses.")

ggplot(data, aes(x = MAEDUC + PAEDUC, y = EDUC)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Plot of RINCOME ~ INCOME",
       x = "MAEDUC + PAEDUC",
       y = "EDUC")

model_income_edu <- lm(RINCOME ~ EDUC, data)

summary(model_income_edu)
modelsummary(model_income_edu,
             stars = TRUE,
             notes = "Standard errors in parentheses.")

ggplot(data, aes(x = EDUC, y = RINCOME)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(title = "Regression Plot of RINCOME ~ EDUC",
       x = "EDUC",
       y = "RINCOME")


