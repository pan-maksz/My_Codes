#####################################################################
## Влияние отраслевой принадлежности российских публичных компаний ##
## на формирование их структуры капитала                           ##                                              ##
#####################################################################

# библиотеки
# install.packages("zoo") # установка пакетов
library(dplyr)
library(stargazer) # красивые таблицы
library(readxl) # для чтения excel файла
library(plm) # работа с панельными данными
library(doBy) # summarizing data by groups
library(zoo) 
library('ggplot2')
library("corrplot")
library(writexl)
library("lmtest")
library("sandwich")
#### Данные ####

# загрузим датасет
data <- read_excel("C:/Users/shelk/Downloads/data_r_1.xlsx")

comp_means <- 
  data %>%
  group_by(company) %>%
  summarize(DE_market = mean(DE_market, na.rm = TRUE ),
            industry = unique(industry),
            ROA_net = mean(ROA_operating, na.rm = TRUE),
            SIZE = mean(SIZE_assets, na.rm = TRUE),
            GROWTH = mean(growth_assets, na.rm = TRUE),
            VAR_SALES = mean(Var_sales, na.rm = TRUE),
            GOVERNMENT = mean(government, na.rm = TRUE),
            TAX = mean(ETR, na.rm = TRUE),
            TANGIBILITY = mean(tangibility, na.rm = TRUE)
  )

describe<- as.data.frame(select(comp_means, - company) %>%
                           group_by(industry) %>%
                           summarize_all(.funs = mean))

# описательная статистика всего датасета
stargazer(as.data.frame(data[,- c(1:3)]), type = 'html')

#### Преобразование переменных ####

plot(data$DE_market)
# прологарифмируем DE
data$DE_market_log <- log(1+data$DE_market*100)
plot(data$DE_market_log)
# для логарифмирования используем формулу log(1 + x), так как переменная принимает низкие значения

plot(data$SIZE_assets)
# прологарифмируем SIZE
data$SIZE_assets_log <- log(data$SIZE_assets)
plot(data$SIZE_assets_log)
plot(data$tangibility)

# Заполним Var
var_filled <- na.locf(data$Var_sales[data$year == 2020])
var_filled <- setNames(var_filled, unique(data$company))
data$Var_sales <- var_filled[match(data$company, names(var_filled))]


#### Описательная статистика 2 ####
# Calculate summary statistics by year
summary_by_year <- data %>%
  group_by(year) %>%
  summarize(mean = mean(DE_market , na.rm = TRUE),
            sd = sd(DE_market , na.rm = TRUE),
            min = min(DE_market , na.rm = TRUE),
            max = max(DE_market , na.rm = TRUE))

# корреляционная матрица для всего пула
correlations <- cor(data[, -c(1:3,6,9)], use = "pairwise.complete.obs")
corrplot(correlations)
print(correlations)

# корреляционная матрица для средних по компаниям
comp_means <- 
  data[, -c(2:3)] %>%
  group_by(company) %>%
  summarize_all(mean, na.rm=TRUE)
correlations1 <- cor(comp_means[, -c(1,4,7)], use = "pairwise.complete.obs")
corrplot(correlations1)


# описательная статистика для DE_market по отраслям
DE_market_byindustry <- summaryBy(DE_market ~ industry, data = data, FUN = c(mean, sd, min, max),na.rm=TRUE)
print(DE_market_byindustry)



#### Кластерный анализ ####

# Group by industry and calculate the average capital structure over the 5 years
industry_avg_capital_structure <- data %>%
  group_by(industry) %>%
  summarize(avg_capital_structure = mean(DE_market, na.rm = TRUE))
#квантили
quantile(industry_avg_capital_structure$avg_capital_structure, probs = seq(0, 1, 0.25))

data2016<-data[data$year==2016,]
industry_avg_capital_structure2016 <- data2016 %>%
  group_by(industry) %>%
  summarize(avg_capital_structure = mean(DE_market, na.rm = TRUE))
# Divide industries into 5 groups based on the calculated averages
industry_avg_capital_structure2016$group <- cut(
  industry_avg_capital_structure2016$avg_capital_structure,
  breaks = c(-Inf,0.31209067,0.51823199,0.74723358, Inf),
  labels = FALSE,
  include.lowest = TRUE
)

data2018<-data[data$year==2018,]
industry_avg_capital_structure2018 <- data2018 %>%
  group_by(industry) %>%
  summarize(avg_capital_structure = mean(DE_market, na.rm = TRUE))
# Divide industries into 5 groups based on the calculated averages
industry_avg_capital_structure2018$group <- cut(
  industry_avg_capital_structure2018$avg_capital_structure,
  breaks = c(-Inf,0.31209067,0.51823199,0.74723358, Inf),
  labels = FALSE,
  include.lowest = TRUE
)

data2020<-data[data$year==2020,]
industry_avg_capital_structure2020 <- data2020 %>%
  group_by(industry) %>%
  summarize(avg_capital_structure = mean(DE_market, na.rm = TRUE))
# Divide industries into 5 groups based on the calculated averages
industry_avg_capital_structure2020$group <- cut(
  industry_avg_capital_structure2020$avg_capital_structure,
  breaks = c(-Inf,0.31209067,0.51823199,0.74723358, Inf),
  labels = FALSE,
  include.lowest = TRUE
)

# Print the result
as.data.frame(industry_avg_capital_structure2016)[order(-industry_avg_capital_structure2016$group),]
as.data.frame(industry_avg_capital_structure2018)[order(-industry_avg_capital_structure2018$group),]
as.data.frame(industry_avg_capital_structure2020)[order(-industry_avg_capital_structure2020$group),]


#### Иерархическая кластеризация ####
#2016
#Обнаружены выбросы: Инград, ПИК и НКХП

library(tidyr)
data2016<-data[data$year==2016 & data$company!='PIK' & data$company!='NKXP' & data$company!='INGRAD'& data$company!='VEON'& data$company!= 'unipro',]
vars <- c("DE_market_log", 'SIZE_assets_log','ROA_operating', 'Var_sales', 'tangibility','growth_assets')
data2016[, vars] <- scale(data2016[, vars])

# Calculate distance matrix
dist_matrix <- dist(data2016[, vars], method = "euclidean")

# Perform hierarchical clustering using complete linkage method
hc <- hclust(dist_matrix, method = "complete")
plot(hc)

# Select number of clusters
k <- 3
clusters <- cutree(hc, k)

# Add cluster labels to data

data2016$cluster <- clusters

# Summarize the number of companies per industry in each cluster
summary_table <- data2016 %>%
  group_by(industry, cluster) %>%
  summarize(n = n()) %>%
  spread(cluster, n, fill = 0)

summary_table2 <-
  data2016[, -c(1,2,3, 12, 13)] %>%
  group_by(cluster) %>%
  summarize_all(mean, na.rm=TRUE)

anova<-aov(data=data2016, DE_market_log ~  cluster)
summary(anova)
anova<-aov(data=data2016, tangibility ~ cluster)
summary(anova)
anova<-aov(data=data2016, Var_sales ~ cluster)
summary(anova)
anova<-aov(data=data2016, SIZE_assets_log ~ cluster)
summary(anova)
anova<-aov(data=data2016, ROA_operating ~ cluster)
summary(anova)
anova<-aov(data=data2016, growth_assets ~ cluster)
summary(anova)
anova<-aov(data=data2016, government ~ cluster)
summary(anova)

#2020
data2020<-data[data$year==2020 & data$company !='mostotrest'& data$company !='raspadskia',]
vars <- c("DE_market_log", 'SIZE_assets_log','ROA_operating', 'Var_sales', 'tangibility',  'government')
data2020[, vars] <- scale(data2020[, vars])

# Calculate distance matrix
dist_matrix <- dist(data2020[, vars], method = "euclidean")

# Perform hierarchical clustering using complete linkage method
hc <- hclust(dist_matrix, method = "complete")
plot(hc)

# Select number of clusters
  k <-3 
clusters <- cutree(hc, k)

# Add cluster labels to data
data2020$cluster <- clusters

# Summarize the number of companies per industry in each cluster
summary_table1 <- data2020 %>%
  group_by(industry, cluster) %>%
  summarize(n = n()) %>%
  spread(cluster, n, fill = 0)
summary_table1

# Summarize the clusters
summary_table2 <-
  data2020[, -c(1,2,3, 12, 13)] %>%
  group_by(cluster) %>%
  summarize_all(mean, na.rm=TRUE)


anova<-aov(data=data2020, DE_market_log ~  cluster)
summary(anova)
anova<-aov(data=data2020, tangibility ~ cluster)
summary(anova)
anova<-aov(data=data2020, Var_sales ~ cluster)
summary(anova)
anova<-aov(data=data2020, SIZE_assets_log ~ cluster)
summary(anova)
anova<-aov(data=data2020, ROA_operating ~ cluster)
summary(anova)
anova<-aov(data=data2020, growth_assets ~ cluster)
summary(anova)
anova<-aov(data=data2020, government ~ cluster)
summary(anova)

#### Регрессии для DE_market ####
reg_market_log_inds <- DE_market_log ~ factor(industry)
FE1_market_log <- DE_market_log ~  ROA_operating  + SIZE_assets_log + log(1+Var_sales*100) + ETR + government + tangibility + factor(industry)
reg_market_log_withoutind <- DE_market_log ~  ROA_operating + SIZE_assets_log + log(1+Var_sales*100)+ ETR  + government + tangibility 

# Регрессия пула
m.reg_market_log_inds <- lm(reg_market_log_inds, 
                             data = data)
stargazer(m.reg_market_log_inds, type="html")


m.Pooling_market_log_without <- lm(reg_market_log_withoutind, 
                            data = data)
#фиксированные эффекты отрасли
m.FE1_market_log <- lm(FE1_market_log, data = data)
stargazer(m.FE1_market_log, m.Pooling_market_log_without, type="text")

#ridge
library(glmnet)
data1 <- na.omit(data)
y<-data1$DE_market_log
x<-data.matrix(data1[,c('ROA_operating', 'SIZE_assets_log', 'growth_assets','Var_sales', 'ETR','government','tangibility')])

cv_model <- cv.glmnet(x, y, alpha = 0)
plot(cv_model)
best_lambda <- cv_model$lambda.min
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)
#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)
#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
#find R-Squared
rsq <- 1 - sse/sst
rsq

#lasso
for (lambda in seq(exp(-3.4),exp(-2.8),length.out=100)) {
  mod <- glmnet(x, y, alpha = 1, lambda = lambda)
  print(coef(mod))
}

# Модель с двунаправленнымми фиксированными эффектами
reg_market_log_ind_yr <- DE_market_log ~  ROA_operating  + SIZE_assets_log + log(1+Var_sales*100) + ETR + government + tangibility + factor(industry)+factor(year)
m.FE2_market_log <- lm(data=data, reg_market_log_ind_yr)

stargazer(m.FE2_market_log, type="text")

# Модель со случайными эффектами
m.Random_market_log <- plm(reg_market_log_withoutind, 
                       data = data, index=c("industry", "year"),  
                       model = "random") 
stargazer(m.Random_market_log, type="text")

# сравним регрессии

#### Тесты на выбор спецификации ####
library(stats)
anova(m.FE1_market_log,m.Pooling_market_log_without)
#фикс эффекты отрасли значимы на 1%-уровне
anova(m.FE2_market_log,m.FE1_market_log)
#фикс эффекты года  незначимы на 5%-уровне

#бреуш годфри
data<-na.omit(data)
m.FE2_market_log<-plm(reg_market_log_withoutind, data = data,index = c('industry','year'), model = 'within', effect='twoways')
stargazer(m.FE2_market_log, type='text')
plmtest(m.Random_market_log, type = "bp")
#значимые случайные эффекты

#Тест Хаусмана для сравнения FE и RE моделей
# H0: RE состоятельна 
# H1: FE лучше
phtest(m.FE2_market_log, m.Random_market_log)
# p-value > 0.05 => FE

#### Все таблицы ####
robust_se1<- sqrt(diag(vcovHC(m.Pooling_market_log_without, "HC1")))
robust_se2<- sqrt(diag(vcovHC(m.FE1_market_log, "HC1")))
robust_se3<- sqrt(diag(vcovHC(m.FE2_market_log, "HC1")))
stargazer(m.Pooling_market_log_without, m.FE1_market_log, m.FE2_market_log, m.Random_market_log, 
          type="html", 
          column.labels=c("Без эффектов отрасли ","Фикс. эффекты отрасли", "Эффекты отрасли и года", "Случайные эффекты"),
          se = list(robust_se1, robust_se2, robust_se3, NULL),
          digits=3)



#тестим отдельно для роста компании
m.growt_pooled<-lm(data=data, DE_market_log ~  ROA_operating + growth_assets + log(1+Var_sales*100)+ ETR  + government + tangibility)
summary(m.growt_pooled)
m.growth_FE_ind<-lm(data=data, DE_market_log ~  ROA_operating + growth_assets + log(1+Var_sales*100)+ ETR  + government + tangibility + factor(industry))
summary(m.growth_FE_ind)
m.growth_FE_ind_yr<-lm(data=data, DE_market_log ~  ROA_operating + growth_assets + log(1+Var_sales*100)+ ETR  + government + tangibility + factor(industry)+factor(year))
summary(m.growth_FE_ind_yr)
m.Random_market_log <- plm(data=data, DE_market_log ~  ROA_operating + growth_assets + log(1+Var_sales*100)+ ETR  + government + tangibility, index=c("industry", "year"),  
                           model = "random") 
summary(m.Random_market_log)
robust_se1<- sqrt(diag(vcovHC(m.growt_pooled, "HC1")))
robust_se2<- sqrt(diag(vcovHC(m.growth_FE_ind, "HC1")))
robust_se3<- sqrt(diag(vcovHC(m.growth_FE_ind_yr, "HC1")))
stargazer(m.growt_pooled, m.growth_FE_ind, m.growth_FE_ind_yr, m.Random_market_log, 
          type="html", 
          column.labels=c("Без эффектов отрасли ","Фикс. эффекты отрасли", "Эффекты отрасли и года", "Случайные эффекты"),
          se = list(robust_se1, robust_se2, robust_se3, NULL),
          digits=3)

anova(m.growth_FE_ind,m.growt_pooled)
#фикс эффекты отрасли значимы на 1%-уровне
anova(m.growth_FE_ind_yr,m.growth_FE_ind)
#фикс эффекты года  незначимы на 5%-уровне

#бреуш годфри
data<-na.omit(data)
m.growth_FE_ind_yr<-plm(DE_market_log ~  ROA_operating + growth_assets + log(1+Var_sales*100)+ ETR  + government + tangibility, data = data,index = c('industry','year'), model = 'within', effect='twoways')
stargazer(m.FE2_market_log, type='text')
plmtest(m.Random_market_log, type = "bp")
#значимые случайные эффекты

#Тест Хаусмана для сравнения FE и RE моделей
# H0: RE состоятельна 
# H1: FE лучше
phtest(m.growth_FE_ind_yr, m.Random_market_log)
# p-value > 0.05 => FE


#2МНК
data$ROA_lag<-0
for (yr in 2017:2021) {
  data[data$year==yr,]$ROA_lag<-data[data$year==(yr-1),]$ROA_operating
}
data[data$year==2016,]$ROA_lag<-NA
#1st step
firstmod<-lm(ROA_operating ~ ROA_lag + SIZE_assets_log + log(1+Var_sales*100) + ETR + government + tangibility, data = data)
stargazer(firstmod, type = 'html')
data$ROA_hat<-predict(firstmod, newdata = data)
#2nd step
ivmodel<-plm(DE_market_log ~  ROA_hat  + SIZE_assets_log + log(1+Var_sales*100) + ETR + government + tangibility + factor(industry), data = data, model = 'pooling')
summary(ivmodel)
stargazer(ivmodel, type = 'html')
OLSmodel<-plm(DE_market_log ~  ROA_operating  + SIZE_assets_log + log(1+Var_sales*100) + ETR + government + tangibility + factor(industry), data = data[data$year != 2016,], model = 'pooling')
summary(OLSmodel)

#hausman test
phtest(ivmodel,OLSmodel)
#relevancy
testmod<-lm(ROA_operating ~ SIZE_assets_log + log(1+Var_sales*100) + ETR + government + tangibility, data = data[data$year!=2016,])
anova(firstmod, testmod)
#МНК лучше


#dynamic
data$DE_change<-0
for (yr in 2017:2021) {
  data[data$year==yr,]$DE_change<- (data[data$year==yr,]$DE_market_log - data[data$year==(yr-1),]$DE_market_log)
}
data<-na.omit(data)
data$DE_hat<-predict(m.FE1_market_log, data=data)
data[data$year==2016,]$DE_change<-NA

dynmod0<-lm(DE_change ~ 0 + I(DE_hat - DE_market_log), data=data)
summary(dynmod0)
dynmod1<-lm(DE_change ~ 0 + I(DE_hat - DE_market_log)+ factor(industry)*I(DE_hat - DE_market_log) - factor(industry), data=data)
summary(dynmod1)
anova(dynmod0,dynmod1)
stargazer(dynmod0, dynmod1, type = 'html')

dynmod2<-lm(DE_change ~ 0 + I(DE_hat - DE_market_log)
            + I(I(DE_hat - DE_market_log)*ROA_operating)
            + I(I(DE_hat - DE_market_log)*SIZE_assets_log)
            + I(I(DE_hat - DE_market_log)*government)
            + I(I(DE_hat - DE_market_log)*tangibility)
            + I(I(DE_hat - DE_market_log)*growth_assets)
            + I(I(DE_hat - DE_market_log)*Var_sales)
            + I(I(DE_hat - DE_market_log)*ETR), data=data)
summary(dynmod2)
robust_se<- sqrt(diag(vcovHC(dynmod2, "HC1")))
stargazer(dynmod2, type='html', se=list(robust_se))
