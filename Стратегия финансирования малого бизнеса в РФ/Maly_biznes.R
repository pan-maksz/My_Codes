library("dplyr")
library("ggplot2")
library(readxl)
library(tidyr)
library(stargazer)
library("factoextra")
library("corrplot")
library(AER)
library(lmtest)
library(stats)
library(car)
library(plm)
library(ggpubr)

rm(list=ls())

spark_data <- na.omit(read_excel("C:/Users/shelk/Desktop/spark_data.xlsx"))

data2018<-select(spark_data, c(1,2,3,4,9,14,19,24,29,34))
data2019<-select(spark_data, c(1,2,3,5,10,15,20,25,30,35))
data2020<-select(spark_data, c(1,2,3,4,11,16,20,26,31,36))
data2021<-select(spark_data, c(1,2,3,4,12,17,21,27,32,37))
data2022<-select(spark_data, c(1,2,3,4,13,18,22,28,33,38))

data2018['year']<-18
data2019['year']<-19
data2020['year']<-20
data2021['year']<-21
data2022['year']<-22

colnames(data2018)<-c('n','name','id','workers','assets','LTD','STD','revenue','ATR','ROE','year')
colnames(data2019)<-c('n','name','id','workers','assets','LTD','STD','revenue','ATR','ROE','year')
colnames(data2020)<-c('n','name','id','workers','assets','LTD','STD','revenue','ATR','ROE','year')
colnames(data2021)<-c('n','name','id','workers','assets','LTD','STD','revenue','ATR','ROE','year')
colnames(data2022)<-c('n','name','id','workers','assets','LTD','STD','revenue','ATR','ROE','year')
data<-rbind(data2018,data2019,data2020,data2021,data2022)
writexl::write_xlsx(data,"C:/Users/shelk/Desktop/spark_data1.xlsx")

data['SDA']<-data$STD/data$assets
data['LDA']<-data$LTD/data$assets
data['DA']<-data$SDA + data$LDA
data['size']<-0
data['growth']<-0

for (i in 19:22) {
  data[data$year == i, ]$size <-log(data[data$year == i-1, ]$assets)
  data[data$year == i, ]$growth <-data[data$year == i, ]$revenue/data[data$year == i-1, ]$revenue
}
final_data<-data[data$year != 18, 2:16]

#выбросы
#по ROE
plot(final_data$ROE)
IQR <- quantile(final_data$ROE,0.75)-quantile(final_data$ROE,0.25)
plot(final_data[final_data$ROE > quantile(final_data$ROE,0.25)-1.5*IQR & final_data$ROE < quantile(final_data$ROE,0.75)+1.5*IQR, ]$ROE)

final_data<-final_data[final_data$ROE > quantile(final_data$ROE,0.25)-1.5*IQR & final_data$ROE < quantile(final_data$ROE,0.75)+1.5*IQR, ]

#по DA, LDA, SDA
plot(final_data$DA)
IQR <- quantile(final_data$DA,0.75)-quantile(final_data$DA,0.25)
plot(final_data[final_data$DA > quantile(final_data$DA,0.25)-1.5*IQR & final_data$DA < quantile(final_data$DA,0.75)+1.5*IQR, ]$DA)

final_data<-final_data[final_data$DA > quantile(final_data$DA,0.25)-1.5*IQR & final_data$DA < quantile(final_data$DA,0.75)+1.5*IQR, ]

plot(final_data$LDA)
plot(final_data$SDA)

#по growth
plot(final_data$growth)
IQR <- quantile(final_data$growth,0.75)-quantile(final_data$growth,0.25)
plot(final_data[final_data$growth > quantile(final_data$growth,0.25)-1.5*IQR & final_data$growth < quantile(final_data$growth,0.75)+1.5*IQR, ]$growth)
final_data<-final_data[final_data$growth > quantile(final_data$growth,0.25)-1.5*IQR & final_data$growth < quantile(final_data$growth,0.75)+1.5*IQR, ]

#по size всё хорошо
plot(final_data$size)

#по ATR
plot(final_data$ATR)
IQR <- quantile(final_data$ATR,0.75)-quantile(final_data$ATR,0.25)
plot(final_data[final_data$ATR > quantile(final_data$ATR,0.25)-1.5*IQR & final_data$ATR < quantile(final_data$ATR,0.75)+1.5*IQR, ]$ATR)
final_data<-final_data[final_data$ATR > quantile(final_data$ATR,0.25)-1.5*IQR & final_data$ATR < quantile(final_data$ATR,0.75)+1.5*IQR, ]


"
Сбалансируем датасет на всякий случай
"
ids<-final_data %>% group_by(id) %>% summarize(count = n())
final_data_balanced<-final_data[final_data$id %in% ids[ids$count == 4,]$id,]

#итого в полном датасете 3981 наблюдение
#В с балансированном датасете 2516 наблюдений
writexl::write_xlsx(final_data,"C:/Users/shelk/Desktop/empirica/final_data.xlsx")
writexl::write_xlsx(final_data_balanced,"C:/Users/shelk/Desktop/empirica/balanced_data.xlsx")



"
Кластеризация
"
final_data <- read_excel("C:/Users/shelk/Desktop/empirica/final_data.xlsx")

hc <- final_data[,c('SDA','LDA','DA')] %>% scale() %>% dist() %>% hclust()
plot(hc,labels = FALSE)
final_data$cluster <- hc %>% cutree(k=5) %>% as.character()

clust_means <- final_data[,c('cluster', 'SDA','LDA','DA', 'ROE')] %>% group_by(cluster) %>% summarize_all(mean)
writexl::write_xlsx(clust_means,"C:/Users/shelk/Desktop/empirica/кластеры_среднее_1.xlsx")
ggplot(data = final_data, aes(x=SDA,y=LDA, group = cluster, color = cluster))+geom_point()                                                                     



#описательные стаистики + графики
ecm_data<-as.data.frame(final_data[,c('size','ROE','growth','ATR','LDA','SDA','DA')])
stargazer(ecm_data, type = 'html', out = "C:/Users/shelk/Desktop/empirica/описательные_статистики.html")

ggplot(ecm_data,aes(x=DA)) + geom_histogram(color='black', fill='grey') + labs(title='Распределение компаний по Debt to Assets')
ggplot(ecm_data,aes(x=LDA)) + geom_histogram(color='black', fill='grey') + labs(title='Распределение компаний по долгосрочному долгу')
ggplot(ecm_data,aes(x=SDA)) + geom_histogram(color='black', fill='grey') + labs(title='Распределение компаний по краткосрочному долгу')
ggplot(ecm_data,aes(x=ROE)) + geom_histogram(color='black', fill='grey') + labs(title='Распределение компаний по ROE')

#матрица корреляции
ecm_data %>% cor() %>% corrplot(method ='number', sig.level = 0.05)
#Критическое значение
critical.r <- function( n, alpha = .05 ) {
  df <- n - 2
  critical.t <- qt(alpha/2, df, lower.tail = F)
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return(critical.r)
}
critical.r(3981)

rcorr(as.matrix(df))

#pca
data.pca <- ecm_data %>% cor() %>% princomp()
data.pca$loadings[, 1:2]
fviz_pca_var(data.pca, col.var = "black")



#Регрессии
#просто МНК
mod1 <- lm(ROE ~ SDA + growth + size + ATR, data=ecm_data)
mod2 <- lm(ROE ~ LDA + growth + size + ATR, data=ecm_data)
mod3 <- lm(ROE ~ DA + growth + size + ATR, data=ecm_data)
mod4 <- lm(ROE ~ SDA + LDA + growth + size + ATR, data=ecm_data)
stargazer(mod1,mod2,mod3,mod4, type = 'html', out="C:/Users/shelk/Desktop/empirica/обычные_регрессии.html")

#RESET тест Рамсея
resettest(mod1,power=2)
resettest(mod2,power=2)
resettest(mod3,power=2)
resettest(mod4,power=2)
#Везде h0 => хорошие спецификации

#тест Бреуша-Пагана
bptest(mod1)
bptest(mod2)
bptest(mod3)
bptest(mod4)

cse = function(reg) {
  rob = sqrt(diag(vcovHAC(reg)))
  return(rob)
}
stargazer(mod1,mod2,mod3,mod4, type = 'text',
          se = list(cse(mod1),cse(mod2),cse(mod3),cse(mod4)),
          out="C:/Users/shelk/Desktop/empirica/обычные_регрессии_робастные_ошибки.html")

#variance inflation factors
vif(mod1)
vif(mod2)
vif(mod3)
vif(mod4)
#Всё vif меньше 10 => нет мультиколлинеарности


#Фикс эффекты
ecm_data<-as.data.frame(final_data[,c('size','ROE','growth','ATR','LDA','SDA','DA','year','id')])
mod5<-lm(ROE~SDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data)
mod6<-lm(ROE~LDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data)
mod7<-lm(ROE~DA+growth+size+ATR+factor(year)+factor(id), data=ecm_data)
mod8<-lm(ROE~LDA+SDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data)
stargazer(mod5,mod6,mod7,mod8, type = 'html', 
          out="C:/Users/shelk/Desktop/empirica/LSDV_все.html")
anova(mod5,mod1)
anova(mod6,mod2)
anova(mod7,mod3)
anova(mod8,mod4)
#Во всех случаях p-value < 0.01 => Фикс эффекты значимы

#Случайные эффекты
re_mod1 <- plm(ROE~SDA+growth+size+ATR, data = ecm_data, 
                    index = c("id", "year"), 
                    model = "random")
re_mod2 <- plm(ROE~LDA+growth+size+ATR, data = ecm_data, 
               index = c("id", "year"), 
               model = "random")
re_mod3 <- plm(ROE~DA+growth+size+ATR, data = ecm_data, 
               index = c("id", "year"), 
               model = "random")
re_mod4 <- plm(ROE~LDA + SDA+growth+size+ATR, data = ecm_data, 
               index = c("id", "year"), 
               model = "random")
stargazer(re_mod1,re_mod2,re_mod3,re_mod4, type='html',
          out = "C:/Users/shelk/Desktop/empirica/RE_models_все.html")
#FE vs RE
phtest(re_mod1,mod5)
phtest(re_mod2,mod6)
phtest(re_mod3,mod7)
phtest(re_mod4,mod8)
#На 5% лучше фикс эффекты


#Модели для ROE<0
mod_less1<-lm(ROE~SDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE<0,])
mod_less2<-lm(ROE~LDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE<0,])
mod_less3<-lm(ROE~DA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE<0,])
mod_less4<-lm(ROE~LDA+SDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE<0,])
stargazer(mod_less1,mod_less2,mod_less3,mod_less4, type = 'html',
          out="C:/Users/shelk/Desktop/empirica/LSDV_ROE_negative.html")
#Модели для ROE > 0 
mod_high1<-lm(ROE~SDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE>0,])
mod_high2<-lm(ROE~LDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE>0,])
mod_high3<-lm(ROE~DA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE>0,])
mod_high4<-lm(ROE~LDA+SDA+growth+size+ATR+factor(year)+factor(id), data=ecm_data[ecm_data$ROE>0,])
stargazer(mod_high1,mod_high2,mod_high3,mod_high4, type = 'html',
          out="C:/Users/shelk/Desktop/empirica/LSDV_ROE_positive.html")


for(yr in c(22,21,20,19)) {
  #Для 2022-го года, ROE<0
  mod22_less1<-lm(ROE~SDA+growth+size+ATR, data=ecm_data[ecm_data$ROE<0 & ecm_data$year==yr,])
  mod22_less2<-lm(ROE~LDA+growth+size+ATR, data=ecm_data[ecm_data$ROE<0 & ecm_data$year==yr,])
  mod22_less3<-lm(ROE~DA+growth+size+ATR, data=ecm_data[ecm_data$ROE<0 & ecm_data$year==yr,])
  mod22_less4<-lm(ROE~LDA+SDA+growth+size+ATR, data=ecm_data[ecm_data$ROE<0 & ecm_data$year==yr,])
  stargazer(mod22_less1,mod22_less2,mod22_less3,mod22_less4, type='html',
            se = list(cse(mod22_less1),cse(mod22_less2),cse(mod22_less3),cse(mod22_less4)),
            out=paste0("C:/Users/shelk/Desktop/empirica/ROE_negative_20", yr,".html"))
  #2022, ROE > 0
  mod22_high1<-lm(ROE~SDA+growth+size+ATR, data=ecm_data[ecm_data$ROE>0 & ecm_data$year==yr,])
  mod22_high2<-lm(ROE~LDA+growth+size+ATR, data=ecm_data[ecm_data$ROE>0 & ecm_data$year==yr,])
  mod22_high3<-lm(ROE~DA+growth+size+ATR, data=ecm_data[ecm_data$ROE>0 & ecm_data$year==yr,])
  mod22_high4<-lm(ROE~LDA+SDA+growth+size+ATR, data=ecm_data[ecm_data$ROE>0 & ecm_data$year==yr,])
  stargazer(mod22_high1,mod22_high2,mod22_high3,mod22_high4, type='html',
            se = list(cse(mod22_high1),cse(mod22_high2),cse(mod22_high3),cse(mod22_high4)),
            out=paste0("C:/Users/shelk/Desktop/empirica/ROE_positive_20", yr,".html"))
}
anova(mod22_high1,mod22_high4)

#Инструментальные переменные - возьмём лаги долгов 
ecm_data$DA_lag<-NA
for (i in 1:length(ecm_data[,1])){
  try(ecm_data$DA_lag[i]<-ecm_data[ecm_data$id==ecm_data$id[i] & ecm_data$year == ecm_data$year[i]-1,]$DA,silent = TRUE)
}
ecm_data$SDA_lag<-NA
for (i in 1:length(ecm_data[,1])){
  try(ecm_data$SDA_lag[i]<-ecm_data[ecm_data$id==ecm_data$id[i] & ecm_data$year == ecm_data$year[i]-1,]$SDA,silent = TRUE)
}
ecm_data$LDA_lag<-NA
for (i in 1:length(ecm_data[,1])){
  try(ecm_data$LDA_lag[i]<-ecm_data[ecm_data$id==ecm_data$id[i] & ecm_data$year == ecm_data$year[i]-1,]$LDA,silent = TRUE)
}

ecm_data_2sls <- na.omit(ecm_data)
#ROE < 0
ecm_data_2sls_less<-ecm_data_2sls[ecm_data_2sls$ROE<0,]
ecm_data_2sls_less$SDA_hat<-lm(SDA~SDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
ecm_data_2sls_less$LDA_hat<-lm(LDA~LDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
ecm_data_2sls_less$DA_hat<-lm(DA~DA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values

IV_mod1<-lm(ROE ~ SDA_hat+growth+ATR+size+factor(year)+factor(id), data=ecm_data_2sls_less)
IV_mod2<-lm(ROE ~ LDA_hat+growth+ATR+size+factor(year)+factor(id),data=ecm_data_2sls_less)
IV_mod3<-lm(ROE ~ DA_hat+growth+ATR+size+factor(year)+factor(id),data=ecm_data_2sls_less)

ecm_data_2sls_less$SDA_hat<-lm(SDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
ecm_data_2sls_less$LDA_hat<-lm(LDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
IV_mod4<-lm(ROE ~ LDA_hat+SDA_hat+growth+ATR+size+factor(year)+factor(id),data=ecm_data_2sls_less)
stargazer(IV_mod1,IV_mod2,IV_mod3,IV_mod4, type='html',
          out="C:/Users/shelk/Desktop/empirica/2SLS_ROE_negative.html")
#F-статистики
summary(lm(SDA~SDA_lag+growth+ATR+size,data=ecm_data_2sls_less))
summary(lm(LDA~LDA_lag+growth+ATR+size,data=ecm_data_2sls_less))
summary(lm(DA~DA_lag+growth+ATR+size,data=ecm_data_2sls_less))
summary(lm(SDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_less))
summary(lm(LDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_less))
#F-статистики везде больше 10 => инструменты релевантные/сильные

#ROE > 0
ecm_data_2sls_high<-ecm_data_2sls[ecm_data_2sls$ROE>0,]
ecm_data_2sls_high$SDA_hat<-lm(SDA~SDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
ecm_data_2sls_high$LDA_hat<-lm(LDA~LDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
ecm_data_2sls_high$DA_hat<-lm(DA~DA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values

IV_mod5<-lm(ROE ~ SDA_hat+growth+ATR+size+factor(year)+factor(id),data=ecm_data_2sls_high)
IV_mod6<-lm(ROE ~ LDA_hat+growth+ATR+size+factor(year)+factor(id),data=ecm_data_2sls_high)
IV_mod7<-lm(ROE ~ DA_hat+growth+ATR+size+factor(year)+factor(id),data=ecm_data_2sls_high)

ecm_data_2sls_high$SDA_hat<-lm(SDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
ecm_data_2sls_high$LDA_hat<-lm(LDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
IV_mod8<-lm(ROE ~ LDA_hat+SDA_hat+growth+ATR+size+factor(year)+factor(id),data=ecm_data_2sls_high)
stargazer(IV_mod5,IV_mod6,IV_mod7,IV_mod8, type='html',
          out="C:/Users/shelk/Desktop/empirica/2SLS_ROE_positive.html")
#F-статистики
summary(lm(SDA~SDA_lag+growth+ATR+size,data=ecm_data_2sls_high))
summary(lm(LDA~LDA_lag+growth+ATR+size,data=ecm_data_2sls_high))
summary(lm(DA~DA_lag+growth+ATR+size,data=ecm_data_2sls_high))
summary(lm(SDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_high))
summary(lm(LDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_high))
#F-статистики везде больше 10 => инструменты релевантные/сильные

#Тест Хаусмана
#посчитаем регрессии для теста на одном датасете, ROE < 0
ecm_data_2sls_less$SDA_hat<-lm(SDA~SDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
ecm_data_2sls_less$LDA_hat<-lm(LDA~LDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
ecm_data_2sls_less$DA_hat<-lm(DA~DA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
#SDA
phtest(plm(ROE~SDA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less),
       plm(ROE~SDA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less))
#LDA
phtest(plm(ROE~LDA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less),
       plm(ROE~LDA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less))
#DA
phtest(plm(ROE~DA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less),
       plm(ROE~DA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less))

ecm_data_2sls_less$SDA_hat<-lm(SDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
ecm_data_2sls_less$LDA_hat<-lm(LDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_less)$fitted.values
#SDA и LDA
phtest(plm(ROE~SDA_hat+LDA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less),
       plm(ROE~SDA+LDA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_less))


#ROE > 0
ecm_data_2sls_high$SDA_hat<-lm(SDA~SDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
ecm_data_2sls_high$LDA_hat<-lm(LDA~LDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
ecm_data_2sls_high$DA_hat<-lm(DA~DA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
#SDA
phtest(plm(ROE~SDA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high),
       plm(ROE~SDA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high))
#LDA
phtest(plm(ROE~LDA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high),
       plm(ROE~LDA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high))
#DA
phtest(plm(ROE~DA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high),
       plm(ROE~DA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high))

ecm_data_2sls_high$SDA_hat<-lm(SDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
ecm_data_2sls_high$LDA_hat<-lm(LDA~SDA_lag+LDA_lag+growth+ATR+size,data=ecm_data_2sls_high)$fitted.values
#SDA и LDA
phtest(plm(ROE~SDA_hat+LDA_hat+growth+ATR+size,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high),
       plm(ROE~SDA+LDA+growth+size+ATR,effect='twoways',index=c('id','year'),model='within', data=ecm_data_2sls_high))

"
На 5% уровне достаточно использовать обычный МНК во всех случаях,кроме модели ROE > 0 для спецификации с DA
"
