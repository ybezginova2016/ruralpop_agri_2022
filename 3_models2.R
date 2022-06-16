setwd("/Users/yuliabezginova/Library/Mobile Documents/com~apple~CloudDocs/Documents/research projects/project 1_С:х показатели и население_22052022")

library(dplyr)
library(readxl)
library(writexl)
library(tidyr)
library(ggplot2)
library(stargazer)

agri <- read_excel("/Users/yuliabezginova/Library/Mobile Documents/com~apple~CloudDocs/Documents/research projects/project 1_С:х показатели и население_22052022/clean_panel.xlsx")
agri <- agri[,-1]

summary(agri)
colnames(agri)

agri$rural_population <- as.numeric(agri$rural_population)
agri$city_population <- as.numeric(agri$city_population)
agri$index <- as.numeric(agri$index)

agri$production <- as.numeric(agri$production)
agri$income <- as.numeric(agri$income)

summary(agri)
agri <- agri[complete.cases(agri),]

##################### REGRESSION MODELS ##################### 

### Linear regression ### 
# Y = rural_population, city_population; X = index, production
lm_rural = lm(data = agri, rural_population ~ production + index + income)
summary(lm_rural)
confint(lm_rural)
library(stargazer)
stargazer(lm_rural, type = "text", digits = 1)

lm_city = lm(data = agri, city_population ~ production + index + income)
summary(lm_city)
confint(lm_city)
library(stargazer)
stargazer(lm_city, type = "text", digits = 1)

### Pooled regression ###
library(plm)
pooled_city = plm(log(city_population) ~ log(production) + log(index) + income,
           data = agri, index = c("year"), model="pooling")
stargazer(pooled_city, type = "text", digits = 1)

pooled_rural = plm(log(rural_population) ~ log(production) + log(index) + income,
             data = agri, index = c("year"), model="pooling")
stargazer(pooled_rural, type = "text", digits = 1)

#Breusch-Pagan Lagram-Multiplier test
plmtest(pooled_city)
plmtest(pooled_rural)

library(lmtest)
bptest(pooled_city)
bptest(pooled_rural)

###  Fixed effects regression ###
# creating unique numbers for regions
d <- agri[c("region_name", "year", "city_population", "rural_population", "index", "production", "income")]
d$id <- seq(1:NROW(d))
agri_id = left_join(agri, d)
agri_id <- data_frame(agri_id)

# modelling
agri_id <- agri_id[complete.cases(agri_id),]
agri_id$rural_population <- as.numeric(agri_id$rural_population)
agri_id$city_population <- as.numeric(agri_id$city_population)
agri_id$index <- as.numeric(agri_id$index)
agri_id$production <- as.numeric(agri_id$production)
agri_id$income <- as.numeric(agri_id$income)

### city_population ###
# fixed effects regression
fixed <- plm(log(city_population) ~ log(index) + log(production) + income, index = c("year"), model = "within", data = agri_id)
summary(fixed)
stargazer(fixed, type ="text")

# random effects regression
random <- plm(log(city_population) ~ log(index) + log(production) + income, index = c("year"), model = "random", data = agri_id)
summary(random)
stargazer(random, type ="text")

phtest(fixed, random)

#Breusch-Pagan Lagram-Multiplier test
plmtest(fixed)
plmtest(random)

# Wald chi2 test for the effect of root
library("lmtest")
waldtest(fixed, random, test = "F")

### rural_population ###

# fixed effects regression
fixed_rural <- plm(log(rural_population) ~ log(index) + log(production) + income, index = c("year"), model = "within", data = agri_id)
summary(fixed_rural)
stargazer(fixed_rural, type ="text")

# random effects regression
random_rural <- plm(log(rural_population) ~ log(index) + log(production) + income, index = c("year"), model = "random", data = agri_id)
summary(random_rural)
stargazer(random_rural, type ="text")

phtest(fixed_rural, random_rural)

#Breusch-Pagan Lagram-Multiplier test
plmtest(fixed_rural)
plmtest(random_rural)

# Wald chi2 test for the effect of root
library("lmtest")
waldtest(fixed_rural, random_rural, test = "F")
