library(dplyr)
library(readxl)
library(xlsx)   

setwd("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022")

#Читаем данные численности сельского населения в общей численности населения"
rural_2014 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/2014.xlsx") 
rural_2015 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/2015.xlsx") 
rural_2016 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/2016.xlsx") 
rural_2017 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/2017.xlsx") 
rural_2018 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/2018.xlsx") 
rural_2019 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/2019.xlsx") 
rural_2020 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/2020.xlsx") 

# removing 2, 3 columns
rural_2014 <- rural_2014[,-c(2:3)]
rural_2015 <- rural_2015[,-c(2:3)]
rural_2016 <- rural_2016[,-c(2:3)]
rural_2017 <- rural_2017[,-c(2:3)]
rural_2018 <- rural_2018[,-c(2:3)]
rural_2019 <- rural_2019[,-c(2:3)]
rural_2020 <- rural_2020[,-c(2:3)]

# merging rural shares datasets 2014-2020
rural <- left_join(rural_2020, rural_2019)
rural <- left_join(rural, rural_2018)
rural <- left_join(rural, rural_2017)
rural <- left_join(rural, rural_2016)
rural <- left_join(rural, rural_2015)
rural <- left_join(rural, rural_2014)

# cleaning the data
rural <- rural[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", rural$region_name),]
rural <- rural[!grepl("Российская Федерация", rural$region_name),]
rural <- rural[!grepl("федеральный округ", rural$region_name),]
rural <- rural[!grepl("в том числе:", rural$region_name),]

rural$region_name[(rural$region_name) == "Город Москва столица Российской Федерации город федерального значения"] <- "Москва"
rural$region_name[(rural$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
rural$region_name[(rural$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
rural$region_name[(rural$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
rural$region_name[(rural$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
rural$region_name[(rural$region_name) == "Тюменская область без автономных округов"] <- "Тюменская область"
rural$region_name[(rural$region_name) == "г. Севастополь"] <- "Севастополь"
rural$region_name[(rural$region_name) == "г. Москва"] <- "Москва"
rural$region_name[(rural$region_name) == "г.Москва"] <- "Москва"
rural$region_name[(rural$region_name) == "г.Санкт-Петербург"] <- "Санкт-Петербург"
rural$region_name[(rural$region_name) == "Архангельская область без автономного округа"] <- "Архангельская область"

# Убираем строки с NA 
# rural <- rural[complete.cases(rural),]

# replacing NAs
rural$"2014"[is.na(rural$"2014")] <- mean(rural$"2014", na.rm = TRUE)
rural$"2015"[is.na(rural$"2015")] <- mean(rural$"2015", na.rm = TRUE)
rural$"2016"[is.na(rural$"2016")] <- mean(rural$"2016", na.rm = TRUE)
rural$"2017"[is.na(rural$"2017")] <- mean(rural$"2017", na.rm = TRUE)
rural$"2018"[is.na(rural$"2018")] <- mean(rural$"2018", na.rm = TRUE)
rural$"2019"[is.na(rural$"2019")] <- mean(rural$"2019", na.rm = TRUE)
rural$"2020"[is.na(rural$"2020")] <- mean(rural$"2020", na.rm = TRUE)

# rural population grouping
library(tidyr)
rural_adj <- gather(rural, key = "year", value = "rural_pop", 2:8)

# creating an xlsx spreadsheet
write.xlsx(rural_adj, "rural_adj.xlsx")

#################################################################
# Объем средств государственной поддержки в рамках программ и мероприятий по развитию сельского хозяйства inv # https://www.fedstat.ru/indicator/42373
library(xlsx)
inv <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/inv.xls")
inv <- inv[, -2]

# renaming columns
inv$region_name[(inv$region_name) == "г. Москва"] <- "Москва"
inv$region_name[(inv$region_name) == "Город Москва столица Российской Федерации город федерального значения"] <- "Москва"
inv$region_name[(inv$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
inv$region_name[(inv$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
inv$region_name[(inv$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
inv$region_name[(inv$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
inv$region_name[(inv$region_name) == "г. Севастополь"] <- "Севастополь"
inv$region_name[(inv$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
inv$region_name[(inv$region_name) == "г. Санкт-Петербург"] <- "Санкт-Петербург"
inv$region_name[(inv$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
inv$region_name[(inv$region_name) == "Архангельская обл. без данных по Ненецкому авт. окр."] <- "Архангельская область"
inv$region_name[(inv$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
inv$region_name[(inv$region_name) == "Ханты-Мансийский автономный округ - Югра (Тюменская область)"] <- "Ханты-Мансийский автономный округ - Югра"
inv$region_name[(inv$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"

# Deleting strings
inv <- inv[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", inv$region_name),]
inv <- inv[!grepl("Российская Федерация", inv$region_name),]
inv <- inv[!grepl("федеральный округ", inv$region_name),]
inv <- inv[!grepl("в том числе:", inv$region_name),]

# Убираем строки с NA 
# inv <- inv[complete.cases(inv),]

# as numeric
inv$"2014" <- as.numeric(inv$"2014")
inv$"2015" <- as.numeric(inv$"2015")
inv$"2016" <- as.numeric(inv$"2016")
inv$"2017" <- as.numeric(inv$"2017")
inv$"2018" <- as.numeric(inv$"2018")
inv$"2019"[(inv$"2019" == "–")] <- NA
inv$"2019" <- as.numeric(inv$"2019")
inv$"2020"[(inv$"2020" == "–")] <- NA
inv$"2020" <- as.numeric(inv$"2020")

# removing 999999999
inv$"2014"[(inv$"2014") == 999999999] <- NA
inv$"2015"[(inv$"2015") == 999999999] <- NA
inv$"2016"[(inv$"2016") == 999999999] <- NA
inv$"2017"[(inv$"2017") == 999999999] <- NA
inv$"2018"[(inv$"2018") == 999999999] <- NA
inv$"2019"[(inv$"2019") == 999999999] <- NA
inv$"2020"[(inv$"2020") == 999999999] <- NA

# replacing NAs
inv$"2014"[is.na(inv$"2014")] <- mean(inv$"2014", na.rm = TRUE)
inv$"2015"[is.na(inv$"2015")] <- mean(inv$"2015", na.rm = TRUE)
inv$"2016"[is.na(inv$"2016")] <- mean(inv$"2016", na.rm = TRUE)
inv$"2017"[is.na(inv$"2017")] <- mean(inv$"2017", na.rm = TRUE)
inv$"2018"[is.na(inv$"2018")] <- mean(inv$"2018", na.rm = TRUE)
inv$"2019"[is.na(inv$"2019")] <- mean(inv$"2019", na.rm = TRUE)
inv$"2020"[is.na(inv$"2020")] <- mean(inv$"2020", na.rm = TRUE)

# rural population grouping
library(tidyr)
inv_adj <- gather(inv, key = "year", value = "investment", 2:8)

# calculating price average
library(dplyr)
inv_aver <- inv_adj %>%
   group_by(region_name, year) %>% 
   summarise(inv_aver = mean(investment))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(inv_aver), "inv_aver.xlsx")

# Среднемесячная номинальная заработная плата работников в сельском хозяйстве sal
library(xlsx)
sal <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/salary.xls")
# renaming columns
names(sal) <- c("region_name", "month", 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# dropping first 2 lines
sal <- sal[-c(1:2),-2]

# Deleting strings with the word "округ", "в том числе:"
sal <- sal[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", sal$region_name),]
sal <- sal[!grepl("Российская Федерация", sal$region_name),]
sal <- sal[!grepl("федеральный округ", sal$region_name),]
sal <- sal[!grepl("в том числе:", sal$region_name),]

# renaming columns
sal$region_name[(sal$region_name) == "Город Москва столица Российской Федерации город федерального значения"] <- "Москва"
sal$region_name[(sal$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
sal$region_name[(sal$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
sal$region_name[(sal$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
sal$region_name[(sal$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
sal$region_name[(sal$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
sal$region_name[(sal$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
sal$region_name[(sal$region_name) == "Город Санкт-Петербург город федерального значения"] <- "Санкт-Петербург"
sal$region_name[(sal$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
sal$region_name[(sal$region_name) == "Архангельская область (без АО)"] <- "Архангельская область"
sal$region_name[(sal$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
sal$region_name[(sal$region_name) == "Ханты-Мансийский автономный округ - Югра (Тюменская область)"] <- "Ханты-Мансийский автономный округ - Югра"
sal$region_name[(sal$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
sal$region_name[(sal$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
sal$region_name[(sal$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
sal$region_name[(sal$region_name) == "Тюменская область (кроме Ханты-Мансийского автономного округа-Югры и Ямало-Ненецкого автономного округа)"] <- "Тюменская область"

# as numeric
sal$"2014" <- as.numeric(sal$"2014")
sal$"2015" <- as.numeric(sal$"2015")
sal$"2016" <- as.numeric(sal$"2016")
sal$"2017" <- as.numeric(sal$"2017")
sal$"2018" <- as.numeric(sal$"2018")
sal$"2019"[(sal$"2019" == "–")] <- NA
sal$"2019" <- as.numeric(sal$"2019")
sal$"2020"[(sal$"2020" == "–")] <- NA
sal$"2020" <- as.numeric(sal$"2020")

# removing 999999999
sal$"2014"[(sal$"2014") == 999999999] <- NA
sal$"2015"[(sal$"2015") == 999999999] <- NA
sal$"2016"[(sal$"2016") == 999999999] <- NA
sal$"2017"[(sal$"2017") == 999999999] <- NA
sal$"2018"[(sal$"2018") == 999999999] <- NA
sal$"2019"[(sal$"2019") == 999999999] <- NA
sal$"2020"[(sal$"2020") == 999999999] <- NA

# replacing NAs
sal$"2014"[is.na(sal$"2014")] <- mean(sal$"2014", na.rm = TRUE)
sal$"2015"[is.na(sal$"2015")] <- mean(sal$"2015", na.rm = TRUE)
sal$"2016"[is.na(sal$"2016")] <- mean(sal$"2016", na.rm = TRUE)
sal$"2017"[is.na(sal$"2017")] <- mean(sal$"2017", na.rm = TRUE)
sal$"2018"[is.na(sal$"2018")] <- mean(sal$"2018", na.rm = TRUE)
sal$"2019"[is.na(sal$"2019")] <- mean(sal$"2019", na.rm = TRUE)
sal$"2020"[is.na(sal$"2020")] <- mean(sal$"2020", na.rm = TRUE)

# Убираем строки с NA 
# sal <- sal[complete.cases(sal),]

# rural population grouping
library(tidyr)
sal_adj <- gather(sal, key = "year", value = "salary", 2:8)

# calculating price average
library(dplyr)
sal_aver <- sal_adj %>%
   group_by(region_name, year) %>% 
   summarise(sal_aver = mean(salary))

# Убираем строки с NA
# sal_aver %>% drop_na()

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(sal_aver), "sal_aver.xlsx")

# Урожайность сельскохозяйственных культур (в расчете на убранную площадь) prod https://fedstat.ru/indicator/31533 
library("readxl")
prod <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/prod.xls")
prod <- prod[,-c(1:2)]

# renaming columns
names(prod) <- c("region_name", 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# Deleting strings with the word "округ", "в том числе:"
prod <- prod[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", prod$region_name),]
prod <- prod[!grepl("Российская Федерация", prod$region_name),]
prod <- prod[!grepl("федеральный округ", prod$region_name),]
prod <- prod[!grepl("в том числе:", prod$region_name),]

# renaming columns
prod$region_name[(prod$region_name) == "Город Москва столица Российской Федерации город федерального значения"] <- "Москва"
prod$region_name[(prod$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
prod$region_name[(prod$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
prod$region_name[(prod$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"

prod$region_name[(prod$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
prod$region_name[(prod$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
prod$region_name[(prod$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
prod$region_name[(prod$region_name) == "Город Санкт-Петербург город федерального значения"] <- "Санкт-Петербург"
prod$region_name[(prod$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
prod$region_name[(prod$region_name) == "Архангельская область (без АО)"] <- "Архангельская область"
prod$region_name[(prod$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
prod$region_name[(prod$region_name) == "Ханты-Мансийский автономный округ - Югра (Тюменская область)"] <- "Ханты-Мансийский автономный округ - Югра"
prod$region_name[(prod$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
prod$region_name[(prod$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
prod$region_name[(prod$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
prod$region_name[(prod$region_name) == "Тюменская область (кроме Ханты-Мансийского автономного округа-Югры и Ямало-Ненецкого автономного округа)"] <- "Тюменская область"

# as numeric
prod$"2014" <- as.numeric(prod$"2014")
prod$"2015" <- as.numeric(prod$"2015")
prod$"2016" <- as.numeric(prod$"2016")
prod$"2017" <- as.numeric(prod$"2017")
prod$"2018" <- as.numeric(prod$"2018")
prod$"2019"[(prod$"2019" == "–")] <- NA
prod$"2019" <- as.numeric(prod$"2019")
prod$"2020"[(prod$"2020" == "–")] <- NA
prod$"2020" <- as.numeric(prod$"2020")

# removing 999999999
prod$"2014"[(prod$"2014") == 999999999] <- NA
prod$"2015"[(prod$"2015") == 999999999] <- NA
prod$"2016"[(prod$"2016") == 999999999] <- NA
prod$"2017"[(prod$"2017") == 999999999] <- NA
prod$"2018"[(prod$"2018") == 999999999] <- NA
prod$"2019"[(prod$"2019") == 999999999] <- NA
prod$"2020"[(prod$"2020") == 999999999] <- NA

# replacing NAs
prod$"2014"[is.na(prod$"2014")] <- mean(prod$"2014", na.rm = TRUE)
prod$"2015"[is.na(prod$"2015")] <- mean(prod$"2015", na.rm = TRUE)
prod$"2016"[is.na(prod$"2016")] <- mean(prod$"2016", na.rm = TRUE)
prod$"2017"[is.na(prod$"2017")] <- mean(prod$"2017", na.rm = TRUE)
prod$"2018"[is.na(prod$"2018")] <- mean(prod$"2018", na.rm = TRUE)
prod$"2019"[is.na(prod$"2019")] <- mean(prod$"2019", na.rm = TRUE)
prod$"2020"[is.na(prod$"2020")] <- mean(prod$"2020", na.rm = TRUE)

# removing all NAs 
# prod <- prod[complete.cases(prod),]

# grouping
library(tidyr)
prod_adj <- gather(prod, key = "year", value = "production", 2:8)

# calculating price average
prod_aver <- prod_adj %>%
   group_by(region_name, year) %>% 
   summarise(av_production = mean(production))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(prod_aver), "prod_aver.xlsx")

# Внесено сельскохозяйственными организациями органических удобрений fert https://belg.gks.ru/storage/mediabank/YPQgpSV3/0811_2020.htm 

# Наличие сельскохозяйственной техники mach https://fedstat.ru/indicator/33410
mach <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/mach.xls")
mach <- mach[,-1]
names(mach) <- c("region_name", 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# Deleting strings with the word "округ", "в том числе:"
mach <- mach[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", mach$region_name),]
mach <- mach[!grepl("Российская Федерация", mach$region_name),]
mach <- mach[!grepl("федеральный округ", mach$region_name),]
mach <- mach[!grepl("в том числе:", mach$region_name),]

# renaming columns
mach$region_name[(mach$region_name) == "Город Москва столица Российской Федерации город федерального значения"] <- "Москва"
mach$region_name[(mach$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
mach$region_name[(mach$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
mach$region_name[(mach$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
mach$region_name[(mach$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
mach$region_name[(mach$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
mach$region_name[(mach$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
mach$region_name[(mach$region_name) == "Город Санкт-Петербург город федерального значения"] <- "Санкт-Петербург"
mach$region_name[(mach$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
mach$region_name[(mach$region_name) == "Архангельская область (без АО)"] <- "Архангельская область"
mach$region_name[(mach$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
mach$region_name[(mach$region_name) == "Ханты-Мансийский автономный округ - Югра (Тюменская область)"] <- "Ханты-Мансийский автономный округ - Югра"
mach$region_name[(mach$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
mach$region_name[(mach$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
mach$region_name[(mach$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
mach$region_name[(mach$region_name) == "Тюменская область (кроме Ханты-Мансийского автономного округа-Югры и Ямало-Ненецкого автономного округа)"] <- "Тюменская область"

# removing all NAs 
# mach <- mach[complete.cases(mach),]

# as numeric
mach$"2014" <- as.numeric(mach$"2014")
mach$"2015" <- as.numeric(mach$"2015")
mach$"2016" <- as.numeric(mach$"2016")
mach$"2017" <- as.numeric(mach$"2017")
mach$"2018" <- as.numeric(mach$"2018")
mach$"2019"[(mach$"2019" == "–")] <- NA
mach$"2019" <- as.numeric(mach$"2019")
mach$"2020"[(mach$"2020" == "–")] <- NA
mach$"2020" <- as.numeric(mach$"2020")

# removing 999999999
mach$"2014"[(mach$"2014") == 999999999] <- NA
mach$"2015"[(mach$"2015") == 999999999] <- NA
mach$"2016"[(mach$"2016") == 999999999] <- NA
mach$"2017"[(mach$"2017") == 999999999] <- NA
mach$"2018"[(mach$"2018") == 999999999] <- NA
mach$"2019"[(mach$"2019") == 999999999] <- NA
mach$"2020"[(mach$"2020") == 999999999] <- NA

# replacing NAs
mach$"2014"[is.na(mach$"2014")] <- mean(mach$"2014", na.rm = TRUE)
mach$"2015"[is.na(mach$"2015")] <- mean(mach$"2015", na.rm = TRUE)
mach$"2016"[is.na(mach$"2016")] <- mean(mach$"2016", na.rm = TRUE)
mach$"2017"[is.na(mach$"2017")] <- mean(mach$"2017", na.rm = TRUE)
mach$"2018"[is.na(mach$"2018")] <- mean(mach$"2018", na.rm = TRUE)
mach$"2019"[is.na(mach$"2019")] <- mean(mach$"2019", na.rm = TRUE)
mach$"2020"[is.na(mach$"2020")] <- mean(mach$"2020", na.rm = TRUE)

# grouping
library(tidyr)
mach_adj <- gather(mach, key = "year", value = "machine", 2:8)

# calculating price average
mach_aver <- mach_adj %>%
   group_by(region_name, year) %>% 
   summarise(av_mach = mean(machine))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(mach_aver), "mach_aver.xlsx")

# Валовой сбор сельскохозяйственных культур gross https://fedstat.ru/indicator/30950
gross <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/gross.xls")

# renaming columns
names(gross) <- c("region_name", 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# Deleting strings with the word "округ", "в том числе:"
gross <- gross[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", gross$region_name),]
gross <- gross[!grepl("Российская Федерация", gross$region_name),]
gross <- gross[!grepl("федеральный округ", gross$region_name),]
gross <- gross[!grepl("в том числе:", gross$region_name),]

# renaming columns
gross$region_name[(gross$region_name) == "Город Москва столица Российской Федерации город федерального значения"] <- "Москва"
gross$region_name[(gross$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
gross$region_name[(gross$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
gross$region_name[(gross$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
gross$region_name[(gross$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
gross$region_name[(gross$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
gross$region_name[(gross$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
gross$region_name[(gross$region_name) == "Город Санкт-Петербург город федерального значения"] <- "Санкт-Петербург"
gross$region_name[(gross$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
gross$region_name[(gross$region_name) == "Архангельская область (без АО)"] <- "Архангельская область"
gross$region_name[(gross$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
gross$region_name[(gross$region_name) == "Ханты-Мансийский автономный округ - Югра (Тюменская область)"] <- "Ханты-Мансийский автономный округ - Югра"
gross$region_name[(gross$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
gross$region_name[(gross$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
gross$region_name[(gross$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
gross$region_name[(gross$region_name) == "Тюменская область (кроме Ханты-Мансийского автономного округа-Югры и Ямало-Ненецкого автономного округа)"] <- "Тюменская область"

# as numeric
gross$"2014" <- as.numeric(gross$"2014")
gross$"2015" <- as.numeric(gross$"2015")
gross$"2016" <- as.numeric(gross$"2016")
gross$"2017" <- as.numeric(gross$"2017")
gross$"2018" <- as.numeric(gross$"2018")
gross$"2019"[(gross$"2019" == "–")] <- NA
gross$"2019" <- as.numeric(gross$"2019")
gross$"2020"[(gross$"2020" == "–")] <- NA
gross$"2020" <- as.numeric(gross$"2020")

# removing 999999999
gross$"2014"[(gross$"2014") == 999999999] <- NA
gross$"2015"[(gross$"2015") == 999999999] <- NA
gross$"2016"[(gross$"2016") == 999999999] <- NA
gross$"2017"[(gross$"2017") == 999999999] <- NA
gross$"2018"[(gross$"2018") == 999999999] <- NA
gross$"2019"[(gross$"2019") == 999999999] <- NA
gross$"2020"[(gross$"2020") == 999999999] <- NA

# replacing NAs
gross$"2014"[is.na(gross$"2014")] <- mean(gross$"2014", na.rm = TRUE)
gross$"2015"[is.na(gross$"2015")] <- mean(gross$"2015", na.rm = TRUE)
gross$"2016"[is.na(gross$"2016")] <- mean(gross$"2016", na.rm = TRUE)
gross$"2017"[is.na(gross$"2017")] <- mean(gross$"2017", na.rm = TRUE)
gross$"2018"[is.na(gross$"2018")] <- mean(gross$"2018", na.rm = TRUE)
gross$"2019"[is.na(gross$"2019")] <- mean(gross$"2019", na.rm = TRUE)
gross$"2020"[is.na(gross$"2020")] <- mean(gross$"2020", na.rm = TRUE)

# Убираем строки с NA 
# gross <- gross[complete.cases(gross),]
# as.data.frame(gross)

# rural population grouping
library(tidyr)
gross_adj <- gather(gross, key = "year", value = "gross", 2:8)

# calculating price average
gross_aver <- gross_adj %>%
   group_by(region_name, year) %>% 
   summarise(av_gross = mean(gross))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(gross_aver), "gross_aver.xlsx")

# Средние цены производителей сельскохозяйственной продукции, реализованной сельскохозяйственными организациями price
price20142016 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/price20142016.xls")
price20172020 <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/price20172020.xls")
price <- left_join(price20142016, price20172020)

# Deleting strings with the word "округ", "в том числе:"
price <- price[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", price$region_name),]
price <- price[!grepl("Российская Федерация", price$region_name),]
price <- price[!grepl("федеральный округ", price$region_name),]
price <- price[!grepl("в том числе:", price$region_name),]

# renaming columns
price$region_name[(price$region_name) == "Город Москва столица Российской Федерации город федерального значения"] <- "Москва"
price$region_name[(price$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
price$region_name[(price$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
price$region_name[(price$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
price$region_name[(price$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
price$region_name[(price$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
price$region_name[(price$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
price$region_name[(price$region_name) == "Город Санкт-Петербург город федерального значения"] <- "Санкт-Петербург"
price$region_name[(price$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
price$region_name[(price$region_name) == "Архангельская область (без АО)"] <- "Архангельская область"
price$region_name[(price$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
price$region_name[(price$region_name) == "Ханты-Мансийский автономный округ - Югра (Тюменская область)"] <- "Ханты-Мансийский автономный округ - Югра"
price$region_name[(price$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
price$region_name[(price$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
price$region_name[(price$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
price$region_name[(price$region_name) == "Тюменская область (кроме Ханты-Мансийского автономного округа-Югры и Ямало-Ненецкого автономного округа)"] <- "Тюменская область"

# as numeric
price$"2014" <- as.numeric(price$"2014")
price$"2015" <- as.numeric(price$"2015")
price$"2016" <- as.numeric(price$"2016")
price$"2017" <- as.numeric(price$"2017")
price$"2018" <- as.numeric(price$"2018")
price$"2019"[(price$"2019" == "–")] <- NA
price$"2019" <- as.numeric(price$"2019")
price$"2020"[(price$"2020" == "–")] <- NA
price$"2020" <- as.numeric(price$"2020")

# removing 999999999
price$"2014"[(price$"2014") == 999999999] <- NA
price$"2015"[(price$"2015") == 999999999] <- NA
price$"2016"[(price$"2016") == 999999999] <- NA
price$"2017"[(price$"2017") == 999999999] <- NA
price$"2018"[(price$"2018") == 999999999] <- NA
price$"2019"[(price$"2019") == 999999999] <- NA
price$"2020"[(price$"2020") == 999999999] <- NA

# replacing NAs
price$"2014"[is.na(price$"2014")] <- mean(price$"2014", na.rm = TRUE)
price$"2015"[is.na(price$"2015")] <- mean(price$"2015", na.rm = TRUE)
price$"2016"[is.na(price$"2016")] <- mean(price$"2016", na.rm = TRUE)
price$"2017"[is.na(price$"2017")] <- mean(price$"2017", na.rm = TRUE)
price$"2018"[is.na(price$"2018")] <- mean(price$"2018", na.rm = TRUE)
price$"2019"[is.na(price$"2019")] <- mean(price$"2019", na.rm = TRUE)
price$"2020"[is.na(price$"2020")] <- mean(price$"2020", na.rm = TRUE)

# Убираем строки с NA 
# price <- price[complete.cases(price),]

# rural population grouping
library(tidyr)
price_adj <- gather(price, key = "year", value = "price", 2:8)

# calculating price average
price_aver <- price_adj %>%
   group_by(region_name, year) %>% 
   summarise(av_price = mean(price))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(price_aver), "price_aver.xlsx")

# Площадь сельскохозяйственных угодий square https://fedstat.ru/indicator/38136
square <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/square.xls")

# cleaning the data
# Deleting strings with the word "округ", "в том числе:"
square <- square[!grepl("РОССИЙСКАЯ ФЕДЕРАЦИЯ", square$region_name),]
square <- square[!grepl("Российская Федерация", square$region_name),]
square <- square[!grepl("федеральный округ", square$region_name),]
square <- square[!grepl("в том числе:", square$region_name),]

# renaming columns
square$region_name[(square$region_name) == "г. Москва"] <- "Москва"
square$region_name[(square$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
square$region_name[(square$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
square$region_name[(square$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
square$region_name[(square$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
square$region_name[(square$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
square$region_name[(square$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
square$region_name[(square$region_name) == "г. Санкт-Петербург"] <- "Санкт-Петербург"
square$region_name[(square$region_name) == "Еврейская а.о."] <- "Еврейская автономная область"
square$region_name[(square$region_name) == "Забайкальский край (Читинская область)"] <- "Забайкальский край"
square$region_name[(square$region_name) == "Камчатский край (Камчатская область)"] <- "Камчатский край"
square$region_name[(square$region_name) == "Ненецкий а.о."] <- "Ненецкий автономный округ"
square$region_name[(square$region_name) == "Пермский край (Пермская область)"] <- "Пермский край"
square$region_name[(square$region_name) == "Чукотский а.о."] <- "Чукотский автономный округ"
square$region_name[(square$region_name) == "Ямало-Ненецкий а.о."] <- "Ямало-Ненецкий автономный округ"
square$region_name[(square$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
square$region_name[(square$region_name) == "Архангельская область (без АО)"] <- "Архангельская область"
square$region_name[(square$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
square$region_name[(square$region_name) == "Ханты-Мансийский а.о."] <- "Ханты-Мансийский автономный округ - Югра"
square$region_name[(square$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
square$region_name[(square$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
square$region_name[(square$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
square$region_name[(square$region_name) == "Тюменская область (кроме Ханты-Мансийского автономного округа-Югры и Ямало-Ненецкого автономного округа)"] <- "Тюменская область"

# as numeric
square$"2014" <- as.numeric(square$"2014")
square$"2015" <- as.numeric(square$"2015")
square$"2016" <- as.numeric(square$"2016")
square$"2017" <- as.numeric(square$"2017")
square$"2017"[(square$"2017" == "–")] <- NA
square$"2018" <- as.numeric(square$"2018")
square$"2018"[(square$"2018" == "–")] <- NA
square$"2019"[(square$"2019" == "–")] <- NA
square$"2019" <- as.numeric(square$"2019")
square$"2020"[(square$"2020" == "–")] <- NA
square$"2020" <- as.numeric(square$"2020")

# removing 999999999
square$"2014"[(square$"2014") == 999999999] <- NA
square$"2015"[(square$"2015") == 999999999] <- NA
square$"2016"[(square$"2016") == 999999999] <- NA
square$"2017"[(square$"2017") == 999999999] <- NA
square$"2018"[(square$"2018") == 999999999] <- NA
square$"2019"[(square$"2019") == 999999999] <- NA
square$"2020"[(square$"2020") == 999999999] <- NA

# replacing NAs
square$"2014"[is.na(square$"2014")] <- mean(square$"2014", na.rm = TRUE)
square$"2015"[is.na(square$"2015")] <- mean(square$"2015", na.rm = TRUE)
square$"2016"[is.na(square$"2016")] <- mean(square$"2016", na.rm = TRUE)
square$"2017"[is.na(square$"2017")] <- mean(square$"2017", na.rm = TRUE)
square$"2018"[is.na(square$"2018")] <- mean(square$"2018", na.rm = TRUE)
square$"2019"[is.na(square$"2019")] <- mean(square$"2019", na.rm = TRUE)
square$"2020"[is.na(square$"2020")] <- mean(square$"2020", na.rm = TRUE)

# Убираем строки с NA 
# square <- square[complete.cases(square),]

# rural population grouping
library(tidyr)
square_adj <- gather(square, key = "year", value = "square", 2:8)

# calculating price average
square_aver <- square_adj %>%
   group_by(region_name, year) %>% 
   summarise(av_square = mean(square))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(square_aver), "square_aver.xlsx")

### gross regional product
grp <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/vrp_adj.xlsx")

# cleaning the data
grp <- grp[!grepl("Российская Федерация  из суммы областей", grp$region_name),]
grp <- grp[!grepl("Российская Федерация", grp$region_name),]
grp <- grp[!grepl("федеральный округ", grp$region_name),]
grp <- grp[!grepl("в том числе:", grp$region_name),]

# renaming columns
grp$region_name[(grp$region_name) == "г.Москва"] <- "Москва"
grp$region_name[(grp$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
grp$region_name[(grp$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
grp$region_name[(grp$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
grp$region_name[(grp$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
grp$region_name[(grp$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
grp$region_name[(grp$region_name) == "Кемеровская область-Кузбасс"] <- "Кемеровская область"
grp$region_name[(grp$region_name) == "г.Санкт-Петербург"] <- "Санкт-Петербург"
grp$region_name[(grp$region_name) == "г.Севастополь"] <- "Севастополь"
grp$region_name[(grp$region_name) == "Еврейская а.о."] <- "Еврейская автономная область"
grp$region_name[(grp$region_name) == "Забайкальский край (Читинская область)"] <- "Забайкальский край"
grp$region_name[(grp$region_name) == "Камчатский край (Камчатская область)"] <- "Камчатский край"
grp$region_name[(grp$region_name) == "Ненецкий а.о."] <- "Ненецкий автономный округ"
grp$region_name[(grp$region_name) == "Пермский край (Пермская область)"] <- "Пермский край"
grp$region_name[(grp$region_name) == "Чукотский а.о."] <- "Чукотский автономный округ"
grp$region_name[(grp$region_name) == "Ямало-Ненецкий а.о."] <- "Ямало-Ненецкий автономный округ"
grp$region_name[(grp$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
grp$region_name[(grp$region_name) == "Архангельская область без Ненецкого авт. округа"] <- "Архангельская область"
grp$region_name[(grp$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
grp$region_name[(grp$region_name) == "Ханты-Мансийский а.о."] <- "Ханты-Мансийский автономный округ - Югра"
grp$region_name[(grp$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
grp$region_name[(grp$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
grp$region_name[(grp$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
grp$region_name[(grp$region_name) == "Тюменская область (без Ханты-Мансийского авт. округа-Югра и Ямало-Ненецкого авт. округа)"] <- "Тюменская область"

# as numeric
grp$"2014" <- as.numeric(grp$"2014")
grp$"2015" <- as.numeric(grp$"2015")
grp$"2016" <- as.numeric(grp$"2016")
grp$"2017" <- as.numeric(grp$"2017")
grp$"2018" <- as.numeric(grp$"2018")
grp$"2019"[(grp$"2019" == "–")] <- NA
grp$"2019" <- as.numeric(grp$"2019")
grp$"2020"[(grp$"2020" == "–")] <- NA
grp$"2020" <- as.numeric(grp$"2020")

# removing 999999999
grp$"2014"[(grp$"2014") == 999999999] <- NA
grp$"2015"[(grp$"2015") == 999999999] <- NA
grp$"2016"[(grp$"2016") == 999999999] <- NA
grp$"2017"[(grp$"2017") == 999999999] <- NA
grp$"2018"[(grp$"2018") == 999999999] <- NA
grp$"2019"[(grp$"2019") == 999999999] <- NA
grp$"2020"[(grp$"2020") == 999999999] <- NA

# replacing NAs
grp$"2014"[is.na(grp$"2014")] <- mean(grp$"2014", na.rm = TRUE)
grp$"2015"[is.na(grp$"2015")] <- mean(grp$"2015", na.rm = TRUE)
grp$"2016"[is.na(grp$"2016")] <- mean(grp$"2016", na.rm = TRUE)
grp$"2017"[is.na(grp$"2017")] <- mean(grp$"2017", na.rm = TRUE)
grp$"2018"[is.na(grp$"2018")] <- mean(grp$"2018", na.rm = TRUE)
grp$"2019"[is.na(grp$"2019")] <- mean(grp$"2019", na.rm = TRUE)
grp$"2020"[is.na(grp$"2020")] <- mean(grp$"2020", na.rm = TRUE)

# Убираем строки с NA 
# grp <- grp[complete.cases(grp),]

# rural population grouping
library(tidyr)
grp_adj <- gather(grp, key = "year", value = "grp", 2:8)

# calculating price average
grp_aver <- grp_adj %>%
   group_by(region_name, year) %>% 
   summarise(av_grp = mean(grp))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(grp_aver), "grp_aver.xlsx")

# consumer price index
cpi <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/data/cpi.xls")

# cleaning the data
cpi <- cpi[!grepl("Российская Федерация  из суммы областей", cpi$region_name),]
cpi <- cpi[!grepl("Российская Федерация", cpi$region_name),]
cpi <- cpi[!grepl("федеральный округ", cpi$region_name),]
cpi <- cpi[!grepl("в том числе:", cpi$region_name),]

# renaming columns
cpi$region_name[(cpi$region_name) == "г.Москва"] <- "Москва"
cpi$region_name[(cpi$region_name) == "Кемеровская область - Кузбасс"] <- "Кемеровская область"
cpi$region_name[(cpi$region_name) == "Республика Татарстан (Татарстан)"] <- "Республика Татарстан"
cpi$region_name[(cpi$region_name) == "Чувашская Республика - Чувашия"] <- "Чувашская Республика"
cpi$region_name[(cpi$region_name) == "Республика Адыгея (Адыгея)"] <- "Республика Адыгея"
cpi$region_name[(cpi$region_name) == "Город федерального значения Севастополь"] <- "Севастополь"
cpi$region_name[(cpi$region_name) == "Кемеровская область-Кузбасс"] <- "Кемеровская область"
cpi$region_name[(cpi$region_name) == "г.Санкт-Петербург"] <- "Санкт-Петербург"
cpi$region_name[(cpi$region_name) == "г.Севастополь"] <- "Севастополь"
cpi$region_name[(cpi$region_name) == "Еврейская а.о."] <- "Еврейская автономная область"
cpi$region_name[(cpi$region_name) == "Забайкальский край (Читинская область)"] <- "Забайкальский край"
cpi$region_name[(cpi$region_name) == "Камчатский край (Камчатская область)"] <- "Камчатский край"
cpi$region_name[(cpi$region_name) == "Ненецкий а.о."] <- "Ненецкий автономный округ"
cpi$region_name[(cpi$region_name) == "Пермский край (Пермская область)"] <- "Пермский край"
cpi$region_name[(cpi$region_name) == "Чукотский а.о."] <- "Чукотский автономный округ"
cpi$region_name[(cpi$region_name) == "Ямало-Ненецкий а.о."] <- "Ямало-Ненецкий автономный округ"
cpi$region_name[(cpi$region_name) == "Архангельская область (кроме Ненецкого автономного округа)"] <- "Архангельская область"
cpi$region_name[(cpi$region_name) == "Архангельская область без Ненецкого авт. округа"] <- "Архангельская область"
cpi$region_name[(cpi$region_name) == "Тюменская обл.без данных по Ханты-Мансийскому и Ямало-Ненецкому авт. окр."] <- "Тюменская область"
cpi$region_name[(cpi$region_name) == "Ханты-Мансийский а.о."] <- "Ханты-Мансийский автономный округ - Югра"
cpi$region_name[(cpi$region_name) == "Ямало-Ненецкий автономный округ (Тюменская область)"] <- "Ямало-Ненецкий автономный округ"
cpi$region_name[(cpi$region_name) == "Ненецкий автономный округ (Архангельская область)"] <- "Ненецкий автономный округ"
cpi$region_name[(cpi$region_name) == "Тюменская область (без АО)"] <- "Тюменская область"
cpi$region_name[(cpi$region_name) == "Тюменская область (без Ханты-Мансийского авт. округа-Югра и Ямало-Ненецкого авт. округа)"] <- "Тюменская область"

# as numeric
cpi$"2014" <- as.numeric(cpi$"2014")
cpi$"2015" <- as.numeric(cpi$"2015")
cpi$"2016" <- as.numeric(cpi$"2016")
cpi$"2017" <- as.numeric(cpi$"2017")
cpi$"2018" <- as.numeric(cpi$"2018")
cpi$"2019"[(cpi$"2019" == "–")] <- NA
cpi$"2019" <- as.numeric(cpi$"2019")
cpi$"2020"[(cpi$"2020" == "–")] <- NA
cpi$"2020" <- as.numeric(cpi$"2020")

# removing 999999999
cpi$"2014"[(cpi$"2014") == 999999999] <- NA
cpi$"2015"[(cpi$"2015") == 999999999] <- NA
cpi$"2016"[(cpi$"2016") == 999999999] <- NA
cpi$"2017"[(cpi$"2017") == 999999999] <- NA
cpi$"2018"[(cpi$"2018") == 999999999] <- NA
cpi$"2019"[(cpi$"2019") == 999999999] <- NA
cpi$"2020"[(cpi$"2020") == 999999999] <- NA

# replacing NAs
cpi$"2014"[is.na(cpi$"2014")] <- mean(cpi$"2014", na.rm = TRUE)
cpi$"2015"[is.na(cpi$"2015")] <- mean(cpi$"2015", na.rm = TRUE)
cpi$"2016"[is.na(cpi$"2016")] <- mean(cpi$"2016", na.rm = TRUE)
cpi$"2017"[is.na(cpi$"2017")] <- mean(cpi$"2017", na.rm = TRUE)
cpi$"2018"[is.na(cpi$"2018")] <- mean(cpi$"2018", na.rm = TRUE)
cpi$"2019"[is.na(cpi$"2019")] <- mean(cpi$"2019", na.rm = TRUE)
cpi$"2020"[is.na(cpi$"2020")] <- mean(cpi$"2020", na.rm = TRUE)

# Убираем строки с NA 
# cpi <- cpi[complete.cases(cpi),]

# rural population grouping
library(tidyr)
cpi_adj <- gather(cpi, key = "year", value = "cpi", 2:8)

# calculating price average
cpi_aver <- cpi_adj %>%
   group_by(region_name, year) %>% 
   summarise(av_cpi = mean(cpi))

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(cpi_aver), "cpi_aver.xlsx")