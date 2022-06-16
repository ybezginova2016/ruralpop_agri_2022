setwd("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022")

library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(stargazer)

# Y
rural <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/rural_adj.xlsx")

# X
gross <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/gross_aver.xlsx")
inv <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/inv_aver.xlsx")
mach <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/mach_aver.xlsx")
price <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/price_aver.xlsx")
prod <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/prod_aver.xlsx")
sal <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/sal_aver.xlsx")
square <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/square_aver.xlsx")

# control variables
cpi <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/cpi_aver.xlsx")
grp <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/grp_aver.xlsx")

###################### CREATING A PANEL ###################### 

## creating a panel
p <- left_join(rural, gross, c("year", "region_name"))
p1 <- left_join(p, inv, c("year", "region_name"))
p2 <- left_join(p1, mach, c("year", "region_name"))
p3 <- left_join(p2, price, c("year", "region_name"))
p4 <- left_join(p3, prod, c("year", "region_name"))
p5 <- left_join(p4, sal, c("year", "region_name"))
p6 <- left_join(p5, square, c("year", "region_name"))

p7 <- left_join(p6, cpi, c("year", "region_name"))
p_panel <- left_join(p7, grp, c("year", "region_name"))

# creating unique numbers for regions
d <- p_panel[c("region_name", "year", "rural_pop", "av_gross", "inv_aver", "av_mach", "av_price", "av_production", "sal_aver", "av_square", "av_cpi", "av_grp")]
d$id <- seq(1:NROW(d))
panel_id = left_join(p_panel, d)
panel_id <- data_frame(panel_id)

# p_panel <- p_panel[complete.cases(p_panel),]

# creating an xlsx spreadsheet
library(xlsx)
write.xlsx(as.data.frame(panel_id), "panel_id.xlsx")
write.xlsx(as.data.frame(panel_id), "panel_id.xlsx")

library(haven)
write_dta(panel_id, "panel_id.dta")

panel_id <- read_excel("/Users/yuliabezginova/Documents/DS/research_projects/project_1_may2022/panel_id.xlsx")
colnames(panel_id)
panel_id$sal_aver <- as.numeric(panel_id$sal_aver)
panel_id$inv_aver <- as.numeric(panel_id$inv_aver)
panel_id$rural_pop <- as.numeric(panel_id$rural_pop)
panel_id$av_gross <- as.numeric(panel_id$av_gross)
panel_id$av_price <- as.numeric(panel_id$av_price)
panel_id$av_production <- as.numeric(panel_id$av_production)
panel_id$av_square <- as.numeric(panel_id$av_square)
panel_id$av_cpi <- as.numeric(panel_id$av_cpi)
panel_id$av_grp <- as.numeric(panel_id$av_grp)

s <- panel_id %>%
   group_by(year) %>% 
   summarise(rural_pop = mean(rural_pop, na.rm = TRUE),
             salary = mean(sal_aver, na.rm = TRUE),
             price = mean(av_price, na.rm = TRUE),
             gross = mean(av_gross, na.rm = TRUE),
             machine = mean(av_mach, na.rm = TRUE),
             production = mean(av_production, na.rm = TRUE),
             square = mean(av_square, na.rm = TRUE),
             investment = mean(inv_aver, na.rm = TRUE))
summary(s)
s <- s[complete.cases(s),]

# ggplot - ok
library(ggplot2)
ggplot(panel_id, aes(y=rural_pop, x=sal_aver)) + 
   geom_point()+
   geom_smooth(method=lm, se=TRUE,
               color="blue", fill="yellow") +
   xlab("Зарплата, тыс. руб") +
   ylab("Доля сельского населения, %") +
   ggtitle("Зависимость сельского населения от средней зарплаты")

ggplot(panel_id, aes(y=rural_pop, x=av_production)) + 
   geom_point()+
   geom_smooth(method=lm, se=TRUE,
               color="blue", fill="yellow") +
   xlab("Урожайность, ц/га") +
   ylab("Доля сельского населения, %") +
   ggtitle("Зависимость сельского населения от урожайности")

ggplot(panel_id, aes(y=rural_pop, x=av_mach)) + 
   geom_point()+
   geom_smooth(method=lm, se=TRUE,
               color="blue", fill="yellow") +
   xlab("Наличие сельскохозяйственной техники, шт") +
   ylab("Доля сельского населения, %") +
   ggtitle("Зависимость сельского населения от наличия с/х техники")

##### DESCRIPTIVE STATISCTICS #####

# Доля сельского населения, %
ggplot(panel_id) +
   aes(x = rural_pop) +
   geom_histogram() +
   xlab("Доля сельского населения, %")
   # ylab("Доля сельского населения, %") +
   # ggtitle("Зависимость сельского населения от наличия с/х техники")

hist(panel_id$rural_pop, freq = FALSE)
hist(log(panel_id$rural_pop), freq = FALSE)

hist(panel_id$inv_aver, freq = FALSE)
hist(log(panel_id$inv_aver), freq = FALSE)

ggplot(panel_id) +
   aes(x = inv_aver) +
   geom_histogram() +
   xlab("Объем средств государственной поддержки в рамках программ и мероприятий по развитию сельского хозяйства, тыс. руб")

hist(panel_id$av_mach, freq = FALSE)
hist(log(panel_id$av_mach), freq = FALSE)
ggplot(panel_id) +
   aes(x = av_mach) +
   geom_histogram() +
   xlab("Наличие сельскохозяйственной техники, ед.")

ggplot(panel_id) +
   aes(x = av_price) +
   geom_histogram() +
   xlab("Средние цены производителей сельскохозяйственной продукции, реализованной сельскохозяйственными организациями, руб")

ggplot(panel_id) +
   aes(x = av_gross) +
   geom_histogram() +
   xlab("Валовой сбор сельскохозяйственных культур, ц/га")

ggplot(panel_id) +
   aes(x = av_production) +
   geom_histogram() +
   xlab("Урожайность сельскохозяйственных культур (в расчете на убранную площадь), ц/га")

ggplot(panel_id) +
   aes(x = sal_aver) +
   geom_histogram() +
   xlab("Среднемесячная номинальная заработная плата работников в сельском хозяйстве, руб")

# sd, mean, range, quantile

# rural_pop
install.packages("moments")
library(moments)
nrow(filter(panel_id,rural_pop != 0, na.rm = TRUE))
mean(panel_id$rural_pop, na.rm = TRUE)
sd(panel_id$rural_pop, na.rm = TRUE)
var(panel_id$rural_pop, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$rural_pop, 0.25) # first quartile
median(panel_id$rural_pop, na.rm = TRUE) # second quartile
quantile(panel_id$rural_pop, 0.75) # third quartile
skewness(panel_id$rural_pop, na.rm = TRUE)
kurtosis(panel_id$rural_pop, na.rm = TRUE)

# av_gross
nrow(filter(panel_id,av_gross != 0, na.rm = TRUE))
mean(panel_id$av_gross, na.rm = TRUE)
sd(panel_id$av_gross, na.rm = TRUE)
var(panel_id$av_gross, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$av_gross, 0.25) # first quartile
median(panel_id$av_gross, na.rm = TRUE) # second quartile
quantile(panel_id$av_gross, 0.75) # third quartile
skewness(panel_id$av_gross, na.rm = TRUE)
kurtosis(panel_id$av_gross, na.rm = TRUE)

# inv_aver
nrow(filter(panel_id, inv_aver != 0, na.rm = TRUE))
mean(panel_id$inv_aver, na.rm = TRUE)
sd(panel_id$inv_aver, na.rm = TRUE)
var(panel_id$inv_aver, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$inv_aver, 0.25) # first quartile
median(panel_id$inv_aver, na.rm = TRUE) # second quartile
quantile(panel_id$inv_aver, 0.75) # third quartile
skewness(panel_id$inv_aver, na.rm = TRUE)
kurtosis(panel_id$inv_aver, na.rm = TRUE)

# av_price
nrow(filter(panel_id, av_price != 0, na.rm = TRUE))
mean(panel_id$av_price, na.rm = TRUE)
sd(panel_id$av_price, na.rm = TRUE)
var(panel_id$av_price, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$av_price, 0.25) # first quartile
median(panel_id$av_price, na.rm = TRUE) # second quartile
quantile(panel_id$av_price, 0.75) # third quartile
skewness(panel_id$av_price, na.rm = TRUE)
kurtosis(panel_id$av_price, na.rm = TRUE)

# av_mach
nrow(filter(panel_id, av_mach != 0, na.rm = TRUE))
mean(panel_id$av_mach, na.rm = TRUE)
sd(panel_id$av_mach, na.rm = TRUE)
var(panel_id$av_mach, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$av_mach, 0.25) # first quartile
median(panel_id$av_mach, na.rm = TRUE) # second quartile
quantile(panel_id$av_mach, 0.75) # third quartile
skewness(panel_id$av_mach, na.rm = TRUE)
kurtosis(panel_id$av_mach, na.rm = TRUE)

# av_production
nrow(filter(panel_id, av_production != 0, na.rm = TRUE))
mean(panel_id$av_production, na.rm = TRUE)
sd(panel_id$av_production, na.rm = TRUE)
var(panel_id$av_production, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$av_production, 0.25) # first quartile
median(panel_id$av_production, na.rm = TRUE) # second quartile
quantile(panel_id$av_production, 0.75) # third quartile
skewness(panel_id$av_production, na.rm = TRUE)
kurtosis(panel_id$av_production, na.rm = TRUE)

# sal_aver
nrow(filter(panel_id, sal_aver != 0, na.rm = TRUE))
mean(panel_id$sal_aver, na.rm = TRUE)
sd(panel_id$sal_aver, na.rm = TRUE)
var(panel_id$sal_aver, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$sal_aver, 0.25) # first quartile
median(panel_id$sal_aver, na.rm = TRUE) # second quartile
quantile(panel_id$sal_aver, 0.75) # third quartile
skewness(panel_id$sal_aver, na.rm = TRUE)
kurtosis(panel_id$sal_aver, na.rm = TRUE)

# av_square
nrow(filter(panel_id, av_square != 0, na.rm = TRUE))
mean(panel_id$av_square, na.rm = TRUE)
sd(panel_id$av_square, na.rm = TRUE)
var(panel_id$av_square, na.rm = TRUE)
# range(panel_id$rural_pop, na.rm = TRUE)
quantile(panel_id$av_square, 0.25) # first quartile
median(panel_id$av_square, na.rm = TRUE) # second quartile
quantile(panel_id$av_square, 0.75) # third quartile
skewness(panel_id$av_square, na.rm = TRUE)
kurtosis(panel_id$av_square, na.rm = TRUE)


## correlation matrix
cor_agri <- agri[,c("city_population", "rural_population", "index", "production", "income")]

library(plm)
library(dmm)
install.packages("Hmisc")

cor(cor_agri) #correlation matrix

