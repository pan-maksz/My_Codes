rm(list=ls())
library("readxl")
library("dplyr")
library("ggplot2")
library('sandwich')
library("stargazer")
library('lmtest')
library(car)
library("corrplot")

tourism_visa <- read_excel("C:/Users/shelk/Desktop/data_tourism/tourism_visa.xlsx")
fdi <- read_excel("C:/Users/shelk/Desktop/data_tourism/FDI.xlsx")
gdp_growth <- read_excel("C:/Users/shelk/Desktop/data_tourism/gdp_growth.xls")
gdp <- read_excel("C:/Users/shelk/Desktop/data_tourism/gdppc_ppp.xlsx")
population <- read_excel("C:/Users/shelk/Desktop/data_tourism/population.xls")
ppp <- read_excel("C:/Users/shelk/Desktop/data_tourism/PPP.xlsx")
exp_imp <- read_excel("C:/Users/shelk/Desktop/data_tourism/Trade_data.xlsx")


df<-left_join(tourism_visa,gdp,by = c('Year'='year','Country Code'='code'))
df<-left_join(df, gdp_growth, by=c('Year'='year','Country Code'='code'))
df<-left_join(df,population,by=c('Year'='year','Country Code'='Country Code'))
df<-left_join(df,ppp,by=c('Year'='year','Country Code'='code'))
df<-left_join(df,fdi,by=c('Year'='year','Country Code'='Code'))
final<-left_join(df,exp_imp,by=c('Year'='year','Country Code'='Code'))

#remove NAs
data<-na.omit(final)
data<-left_join(data,gdp[gdp$code == 'RUS',c(2,3)],by=c('Year'='year'))
data<-left_join(data,gdp_growth[gdp_growth$code == 'RUS',c(2,3)],by=c('Year'='year'))

#добавим расстояния
library('cepiigeodist')
dist <- select(dist_cepii[dist_cepii$iso_d=='RUS',],c('iso_o','distw'))
data<-left_join(data,dist,by=c('Country Code'='iso_o'))
data$distw<-as.numeric(data$distw)
data <- rename(data,g = g.x, g_rus = g.y, gdppc = gdppc.x, gdppc_rus = gdppc.y)
#добавим фиксированный эффект от ковида
data$covid <- as.numeric(data$Year > 2019)
write.csv(data,"C:/Users/shelk/Desktop/data_tourism/finaldataset.csv")

#language data
data<-read.csv("C:/Users/shelk/Desktop/data_tourism/finaldataset.csv")
lang<-read_excel("C:/Users/shelk/Desktop/data_tourism/language.xlsx")
pop2012 <- read_excel("C:/Users/shelk/Desktop/data_tourism/pop2012.xls")
tab<-left_join(lang,pop2012,by=c('code'='Country Code'))
tab$speakers1<-as.numeric(tab$percent)*tab$pop2012
tab[is.na(tab)] = 0
tab$speakers_adj<-tab$speakers_adj+tab$speakers1
data<-left_join(data,select(tab,c('code','speakers_adj','speakers_wpr')),by=c('Country.Code'='code'))
data[is.na(data)]=0

#добавим полит стабильность
stab<-read_excel("C:/Users/shelk/Desktop/data_tourism/stability_data.xlsx")
data<-left_join(data,stab,by=c('Country.Code'='country_code','Year'='year'))
data$stability<-as.numeric(data$stability)
write.csv(select(data,-c('X')),"C:/Users/shelk/Desktop/data_tourism/finaldataset.csv")


#описательные статистики
stargazer(select(data,-c('Country.Code','Year')),out="C:/Users/shelk/Desktop/data_tourism/описательные статистики.html")
#Корреляцционная матрица
select(data,-c('X','Country.Code','Year')) %>% cor() %>% corrplot(sig.level = 0.05)


#Перейдем к эконометрике
mod1<-lm(I(log(tourism))~I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+g+g_rus+I(log(trade))+invest+ppp+visa, data)
summary(mod1)
#тест Рамсея
resettest(mod1)

#мультиколлинеарность
vif_values<-vif(mod1)
barplot(vif_values, main = "VIF Values", horiz = TRUE, xlim = c(0,12))
abline(v = 10, lwd = 3, lty = 2) 

#тест Бреуша-Пагана
bptest(mod1)
#есть гетероскедастичность

#добавим возможность структурного сдвига из-за ковида
mod2<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+g+g_rus+I(log(trade))+invest+ppp+visa, data)
summary(mod2)
#тест Вальда на длинную и короткую регрессии
waldtest(mod2, mod1)
#длинная лучше

#добавим возможность влиять на коэффициенты
mod3<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+g+g_rus+I(log(trade))+invest+ppp+visa
         + I(covid*log(gdppc))+I(covid*log(gdppc_rus))+I(covid*log(distw))+I(covid*log(pop))
         + I(covid*g)+I(covid*log(trade))+I(covid*invest)
         + I(covid*ppp)+I(covid*visa), data)
summary(mod3)

#модель 2 vs 3
waldtest(mod2, mod3)
#Итог - лучше всего модель 2 с учётом фикс эффекта от ковида
#тесты по mod2
resettest(mod2)
bptest(mod2)
#гетероскедастичность, спецификация правильная

#так как есть гетероскедастичность, добавим робсатные ошибки
cse = function(reg) {
  rob = sqrt(diag(vcovHAC(reg)))
  return(rob)
}

#Модель без приростов ввп
mod4<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa, data)
summary(mod4)
waldtest(mod2, mod4)
waldtest(mod3, mod4)
#короткая лучше

stargazer(mod1,mod2, mod4, type = 'html', 
          out="C:/Users/shelk/Desktop/data_tourism/модели.html")

bptest(mod1)
bptest(mod2)
bptest(mod4)
stargazer(mod1,mod2, mod4, type = 'html', 
          se = list(cse(mod1),cse(mod2), cse(mod4)),
          out="C:/Users/shelk/Desktop/data_tourism/модели_робастные.html")

#модели с языком
mod5<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+g+g_rus+I(log(trade))+invest+ppp+visa+ussr, data)
summary(mod5)
mod6<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+g+g_rus+I(log(trade))+invest+ppp+visa+I(log(speakers_adj+1)), data)
summary(mod6)
mod7<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+g+g_rus+I(log(trade))+invest+ppp+visa+I(log(speakers_wpr+1)), data)
summary(mod7)
stargazer(mod5,mod6,mod7, type = 'html', 
          se = list(cse(mod5),cse(mod6),cse(mod7)),
          out="C:/Users/shelk/Desktop/data_tourism/модели_язык.html")

#модели с языком без роста ВВП
mod8<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+ussr, data)
summary(mod8)
mod9<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+I(log(speakers_adj+1)), data)
summary(mod9)
mod10<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+I(log(speakers_wpr+1)), data)
summary(mod10)
waldtest(mod8, mod5)
waldtest(mod9, mod6)
waldtest(mod10, mod7)
mod<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+ussr+I(log(speakers_wpr+1)), data)
summary(mod)
stargazer(mod8,mod9,mod10,mod, type = 'html', 
          se = list(cse(mod8),cse(mod9),cse(mod10),cse(mod)),
          out="C:/Users/shelk/Desktop/data_tourism/модели_язык_финал.html")

#модели с языком и с учетом полит стабильности
mod11<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+ussr+stability, data)
summary(mod11)
mod12<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+I(log(speakers_adj+1))+stability, data)
summary(mod12)
mod13<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+I(log(speakers_wpr+1))+stability, data)
summary(mod13)
mod14<-lm(I(log(tourism)) ~ covid+I(log(gdppc))+I(log(gdppc_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+ussr+I(log(speakers_wpr+1))+stability, data)
summary(mod14)
stargazer(mod11,mod12,mod13, mod14, type = 'html', 
          se = list(cse(mod11),cse(mod12),cse(mod13),cse(mod14)),
          out="C:/Users/shelk/Desktop/data_tourism/модели_финал.html")
waldtest(mod,mod14)


#протестим самую лучшую модель mod
waldtest(mod,mod4)
waldtest(mod8,mod4)
waldtest(mod9,mod4)
waldtest(mod10,mod4)
vif_values<-vif(mod)
barplot(vif_values, main = "VIF Values", horiz = TRUE, xlim = c(0,12))
abline(v = 10, lwd = 3, lty = 2)
#спецификация и гетероскедастичность
resettest(mod)
bptest(mod)


#проверим на данных с ВВП по ППС вместо ВВП по ППС на душу населения
rgdp_ppp <- read_excel("C:/Users/shelk/Desktop/data_tourism/rgdp_ppp.xls")

data1<-left_join(select(data,-c('X')),rgdp_ppp,by=c('Country.Code'='code','Year'='year'))
data1<-left_join(data1,rgdp_ppp[rgdp_ppp$code=='RUS',c(2,3)],by=c('Year'='year'))
data1 <- rename(data1,rgdp = rgdp.x, rgdp_rus = rgdp.y)

mod14<-lm(I(log(tourism)) ~ covid+I(log(rgdp))+I(log(rgdp_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+ussr+stability, data1)
summary(mod14)
mod15<-lm(I(log(tourism)) ~ covid+I(log(rgdp))+I(log(rgdp_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+I(log(speakers_wpr+1))+stability, data1)
summary(mod15)
mod16<-lm(I(log(tourism)) ~ covid+I(log(rgdp))+I(log(rgdp_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+I(log(speakers_adj+1))+stability, data1)
summary(mod16)
mod17<-lm(I(log(tourism)) ~ covid+I(log(rgdp))+I(log(rgdp_rus))+I(log(distw))+I(log(pop))+I(log(trade))+invest+ppp+visa+ussr+I(log(speakers_adj+1))+stability, data1)
summary(mod17)
stargazer(mod14,mod15,mod16,mod17, type = 'html', 
          se = list(cse(mod14),cse(mod15),cse(mod16),cse(mod17)),
          out="C:/Users/shelk/Desktop/data_tourism/модели_язык_ВВП.html")
