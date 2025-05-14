rm(list=ls())
library('dplyr')
library('ggplot2')
library('stargazer')
library('readxl')
library('writexl')
library('plm')
library('car')
library('corrplot')

covid_data <- read_excel("C:/Users/user/Desktop/Исследование по ковиду/covid_data.xlsx")
covid_support <- read_excel("C:/Users/user/Desktop/Исследование по ковиду/covid_support.xlsx")
credits_FL <- read_excel("C:/Users/user/Desktop/Исследование по ковиду/credits_FL.xlsx")
debt_data <- read_excel("C:/Users/user/Desktop/Исследование по ковиду/debt_data.xlsx")
mean_wage <- read_excel("C:/Users/user/Desktop/Исследование по ковиду/mean_wage.xlsx")

month_covid <- 
  covid_data %>% select(-c('region','region_name','pop')) %>% group_by(month, year) %>% summarize_all(sum)
month_covid$time<-0

i<- -2
for (yr in 2020:2023){
  for (m in 1:12){
    month_covid$time[month_covid$month==m & month_covid$year==yr]<-i
    i<-i+1
  }
}

#графики для месячных данных, которые будем использовать далее
ggplot(month_covid,aes(x=time))+geom_line(aes(y=ill,color='ill'),lwd=1) + geom_line(aes(y=death,color='death'),lwd=1) +
  labs(title = 'Заражения и смертность от COVID-19',y='Количество', x='Месяцы с начала пандемии') +
  scale_color_manual(values = c("red", "blue"), name='Показатель',labels=c('Смерти','Заболевания'))

ggplot(month_covid,aes(x=time))+geom_line(aes(y=ill,color='ill'),lwd=1) + geom_line(aes(y=death,color='death'),lwd=1) +
  scale_y_continuous(trans='log10') +
  labs(title = 'Заражения и смертность от COVID-19', subtitle = 'Логарифмическая шкала',y='Количество', x='Месяцы с начала пандемии') +
  scale_color_manual(values = c("red", "blue"), name='Показатель',labels=c('Смерти','Заболевания'))

#графики для ежедневных данных за интересующий период
covid_daily <- read_excel("C:/Users/user/Desktop/Исследование по ковиду/covid_daily.xlsx")
covid_daily$date<-as.Date(covid_daily$date)

ggplot(covid_daily,aes(x=date))+geom_line(aes(y=illnesses,color='illnesses'),lwd=1) + geom_line(aes(y=deaths,color='deaths'),lwd=1) +
  labs(title = 'Заражения и смертность от COVID-19',y='Количество', x='Дата') +
  scale_color_manual(values = c("red", "blue"), name='Показатель',labels=c('Смерти','Заболевания')) +
  scale_y_continuous(limits=c(0,45000))+
  scale_x_date(limits = as.Date(c('2020-03-12','2021-12-31')))

ggplot(covid_daily,aes(x=date))+geom_line(aes(y=illnesses,color='illnesses'),lwd=1) + geom_line(aes(y=deaths,color='deaths'),lwd=1) +
  labs(title = 'Заражения и смертность от COVID-19',y='Количество', x='Дата') +
  scale_color_manual(values = c("red", "blue"), name='Показатель',labels=c('Смерти','Заболевания'))+
  scale_y_continuous(trans='log10', limits=c(1,45000)) +
  scale_x_date(limits = as.Date(c('2020-03-12','2021-12-31')))+



#Необходимо выделить волны по месяцам
ggplot(covid_daily,aes(x=date))+geom_line(aes(y=illnesses,color='illnesses'),lwd=1) + geom_line(aes(y=deaths,color='deaths'),lwd=1) +
  labs(title = 'Заражения и смертность от COVID-19',y='Количество', x='Дата') +
  scale_color_manual(values = c("red", "blue"), name='Показатель',labels=c('Смерти','Заболевания')) +
  scale_y_continuous(limits=c(0,45000))+
  scale_x_date(limits = as.Date(c('2020-03-12','2021-12-31')))+
  geom_vline(xintercept = as.Date('2020-08-31'))+
  geom_vline(xintercept = as.Date('2021-04-30'))+
  geom_vline(xintercept = as.Date('2021-08-31'))
#Итого:
#1-я волна - до конца августа 2020
#2-я волна - с сентября 2020 по конец апреля 2021
#3-я волна - с мая 2021 по конец августа 2021
#4-я волна - с сентября по декабарь 2022

"
#На всякий случай вот тут ещё с логарифмической шкалой:
ggplot(covid_daily,aes(x=date))+geom_line(aes(y=illnesses,color='illnesses'),lwd=1) + geom_line(aes(y=deaths,color='deaths'),lwd=1) +
  labs(title = 'Заражения и смертность от COVID-19',y='Количество', x='Дата') +
  scale_color_manual(values = c('red', 'blue'), name='Показатель',labels=c('Смерти','Заболевания')) +
  scale_y_continuous(trans='log10', limits=c(1,45000))+
  scale_x_date(limits = as.Date(c('2020-03-12','2021-12-31')))+
  geom_vline(xintercept = as.Date('2020-08-31'))+
  geom_vline(xintercept = as.Date('2021-04-30'))+
  geom_vline(xintercept = as.Date('2021-08-31'))
"

#Интерполируем простым средним - просто соединим три таблички, в которых каждому из месяцев квартала
#отвечает одно общее для квартала число средней зарплаты
mean_wage$month <- (mean_wage$qrt-1)*3+1
mean_wage_int<-mean_wage
for (i in 2:3){
  mean_wage$month <- (mean_wage$qrt-1)*3+i
  mean_wage_int <- mean_wage_int %>% rbind(mean_wage)
}

#Мэтчим таблички
covid_data<- covid_data[covid_data$year<2022,] %>% select(-'region')
fin_data<-left_join(covid_data,debt_data, by=c('region_name'='region','year'='year','month'='month')) %>%
  left_join(select(credits_FL,-'qrt'),by=c('region_name'='region','year'='year','month'='month')) %>%
  left_join(covid_support, by=c('region_name'='region','year'='year','month'='month')) %>%
  left_join(select(mean_wage_int,-'qrt'), by=c('region_name'='region','year'='year','month'='month'))

#уберем пропуски
fin_data <- fin_data %>% na.omit()

#добавим волны ковида, выделенные выше:
#1-я волна - до конца августа 2020
#2-я волна - с сентября 2020 по конец апреля 2021
#3-я волна - с мая 2021 по конец августа 2021
#4-я волна - с сентября по декабарь 2021
fin_data$wave <- 4
fin_data$wave[fin_data$year==2021 & fin_data$month <= 8] <-3
fin_data$wave[fin_data$month <= 4] <-2
fin_data$wave[fin_data$year==2020 & fin_data$month <= 8] <-1
write_xlsx(fin_data,"C:/Users/user/Desktop/Исследование по ковиду/final_data.xlsx")
#итого при исключении пропусков имеем 1826 наблюдений

#таблица средних показателей по волнам
fin_data %>% select(-c('month','year','region_name','pop','lag_debt')) %>% group_by(wave) %>% summarise_all(mean) %>%
  write_xlsx("C:/Users/user/Desktop/Исследование по ковиду/Средние_по_волнам.xlsx")
#таблица разброса (стандартные отклонения показателей по волнам)
fin_data %>% select(-c('month','year','region_name','pop','lag_debt')) %>% group_by(wave) %>% summarise_all(sd) %>%
  write_xlsx("C:/Users/user/Desktop/Исследование по ковиду/Ст_отклонения_по_волнам.xlsx")

#простые описательные статистики
fin_data %>% select(-c('month','year','region_name','pop','wave','lag_debt')) %>% data.frame() %>% 
  stargazer(type='html', omit.summary.stat='n', notes = "N = 1826",
            out = "C:/Users/user/Desktop/Исследование по ковиду/описательные_статистики.xlsx")

#гистограммы распределения
hist(fin_data$wage)
hist(fin_data$pop)
#делаем вывод, что надо использовать логарифмы, так как данные концетрируются в двух бинах, остальное - хвосты
hist(log(fin_data$wage))
hist(log(fin_data$pop))
#теперь распределения более похожи на нормальные


#Преобразуем данные перед эконометрическим анализом
ecm_data<-read_excel("C:/Users/user/Desktop/Исследование по ковиду/final_data.xlsx")

#смертность и заболеваемость - на тысячу человек
ecm_data$death_rate<-1000*ecm_data$death/ecm_data$pop
ecm_data$ill_rate<-1000*ecm_data$ill/ecm_data$pop
#логарифмы доходов и населения
ecm_data$lwage<-log(ecm_data$wage)
ecm_data$lpop<-log(ecm_data$pop)
#кредиты ФЛ на душу населения
ecm_data$credit_rate<-ecm_data$credit/ecm_data$pop

#Корреляционная матрица
ecm_data %>% select(-c('month','year','region_name','wave','qrt','date','pop','ill','death','wage')) %>% 
  cor() %>% corrplot()

#Добавим переменную лаговой заболеваемости - под ней понимаем заболеваемость в предыдущую волну
ecm_data$lag_ill_rate<-0
#заполним лаговую заболеваемость для 2-4 волн
for (wv in 2:4){
  ecm_data$lag_ill_rate[ecm_data$wave==wv]<-mean(ecm_data$ill_rate[ecm_data$wave==wv-1])
}

#Модель пула простая
mod1<-lm(credit_rate ~ ill_rate+death_rate+lwage+debt+lease_delay+fin_support 
         +lease_free+tax_decrease+other_tax_free+lpop, ecm_data)
summary(mod1)
#модель пула с лаговой заболеваемостью
mod2<-lm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lwage+debt+lease_delay
         +fin_support+lease_free+tax_decrease+other_tax_free+lpop, ecm_data)
summary(mod2)

#модель с лаговой заболеваемостью и лаговой смертностью
#сначала создадим лаговую смертность по аналогии
ecm_data$lag_death_rate<-0
#заполним лаговую смертность для 2-4 волн
for (wv in 2:4){
  ecm_data$lag_death_rate[ecm_data$wave==wv]<-mean(ecm_data$death_rate[ecm_data$wave==wv-1])
}

mod3<-lm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lease_delay
         +fin_support+lease_free+tax_decrease+other_tax_free+lpop, ecm_data)
summary(mod3)

#модель с лагом просрочек (debt) - логично предположить, что чем больше было просрочек раньше, тем менее вероятно банк выпустит кредит в текущем периоде
mod4<-lm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage
         +debt+lag_debt+lease_delay
         +fin_support+lease_free+tax_decrease+other_tax_free+lpop, ecm_data)
summary(mod4)

#тесты Бреуша-Пагана на гетероскедастичность:
bptest(mod1)
bptest(mod2)
bptest(mod3)
bptest(mod4)
#Во всех случаях есть гетероскедастичность, используем HAC-ошибки

cse <- function(mod){
  sqrt(diag(vcovHC(mod)))
}
stargazer(mod1,mod2,mod3,mod4,
          se=list(cse(mod1),cse(mod2),cse(mod3),cse(mod4)),
          type='html', 
          out="C:/Users/user/Desktop/Работки/Исследование по ковиду/Модели пула.html")

#Лучший R-квадрат - в модели 3, сравним её с моделью 1 по F-тесту короткой и длинной регрессии
anova(mod1,mod3)
#итог - добавление лагов заболеваемости и смертности на 1% уровне улучшает модель

#Фиксированные эффекты

#1 фиксированные эффекты региона - при этом нет смысла включать население, так как оно с минимальной погрешностью постоянно для регионов
mod_FE1<-lm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lease_delay
                  +fin_support+lease_free+tax_decrease+other_tax_free
                  +factor(region_name), ecm_data)
summary(mod_FE1)

#2 фиксированные эффекты месяца (может, есть некоторая цикличность в выдаче кредитов)
mod_FE2<-lm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lease_delay
            +fin_support+lease_free+tax_decrease+other_tax_free+lpop
            +factor(month), ecm_data)
summary(mod_FE2)

#Проверим значимость фиксированных эффектов
anova(mod_FE1,mod3)
anova(mod_FE2,mod3)
#Оба фиксированных эффекта значимы, т.к. p-val<0.01

#проверим значимость лага задолженности при учете временных эффектов (цикличности)
mod_FE21<-lm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lag_debt+lease_delay
            +fin_support+lease_free+tax_decrease+other_tax_free+lpop
            +factor(month), ecm_data)
summary(mod_FE21)

#Двунаправленная модель - с эффектами и региона, и времени
mod_FE3<-lm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lag_debt+lease_delay
            +fin_support+lease_free+tax_decrease+other_tax_free
            +factor(region_name)+factor(month), ecm_data)
summary(mod_FE3)

#F-тест на необходимость двунаправленной модели
anova(mod_FE3,mod_FE1)
anova(mod_FE3,mod_FE2)
#Итог - наилучшая модель - с фиксированными эффектами региона и месяца

#Тест на гетероскедастичность
bptest(mod_FE3)
#Гетероскедастичность есть

stargazer(mod_FE1,mod_FE21,mod_FE3, 
          se=list(cse(mod_FE1),cse(mod_FE21),cse(mod_FE3)),
          keep=c('ill_rate','lag_ill_rate','death_rate','lag_death_rate,','lwage','debt','lag_debt', 
                 'lease_delay','fin_support','lease_free','tax_decrease','other_tax_free','(Intercept)'),
          column.labels = c('region FE','month FE','region&month FE'),
          type='html', out = "C:/Users/user/Desktop/Работки/Исследование по ковиду/Модели фикс эффектов.html")

"
Добавлять фиксированные эффекты волны нет смысла, так как такими фиксированными
эффектами являются лаговые показатели рождаемости и смертности для каждой волны.
Это может привести к строгой мультиколлинеарности!
"


#random effects
#Случайные эффекты региона
mod_RE1<-plm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lease_delay
             +fin_support+lease_free+tax_decrease+other_tax_free, 
             index=c('region_name','month'), effect = 'individual', model = 'random',
             data=ecm_data)

#Случайные vs Фиксированные эффекты
phtest(mod_RE1,mod_FE1)
#p-value < 0.01 => лучше использовать фиксированные эффекты

#случайные эффекты месяца (нужно, чтобы были полные даты для оценки модели - создадим их)
ecm_data$date <- paste(ecm_data$year,ecm_data$month, sep='-')
#RE
mod_RE2<-plm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+lag_debt+debt+lease_delay
             +fin_support+lease_free+tax_decrease+other_tax_free, 
             index=c('region_name','date'), effect = 'time', model = 'random',
             data=ecm_data)
#FE для полных дат
mod_FE22<-plm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lag_debt+lease_delay
                       +fin_support+lease_free+tax_decrease+other_tax_free, 
                       index=c('region_name','date'), effect = 'time', model = 'pooling',
                       data=ecm_data)
#Сравним случайные vs фиксированные
phtest(mod_FE22,mod_RE2)
#на 5% уровне принимаем альтернативную гипотезу о том, что фиксированные эффекты лучше

mod_RE3<-plm(credit_rate ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lag_debt+lease_delay
             +fin_support+lease_free+tax_decrease+other_tax_free, 
             index=c('region_name','date'), effect = 'twoways', model = 'random',
             data=ecm_data)

stargazer(mod_RE1,mod_RE2,mod_RE3, 
          se=list(cse(mod_RE1),cse(mod_RE2),cse(mod_RE3)),
          column.labels = c('region RE','month RE','region&month RE'),
          type='html', out = "C:/Users/user/Desktop/Работки/Исследование по ковиду/Модели случайных эффектов.html")

"
Остановимся на модели со случайными эффектами месяца и региона
"

#Тест Бокса-Кокса для проверки необходимости преобразования зависимой переменной
boxCox(mod_FE3)
#лямбда близка к нулю, можно прологарифмировать зависимую переменную

mod_FE4<-lm(I(log(credit_rate)) ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lag_debt+lease_delay
            +fin_support+lease_free+tax_decrease+other_tax_free+lpop
            +factor(region_name)+factor(month), ecm_data)
summary(mod_FE4)

#график частичных остатков
crPlots(mod_FE4)

#вероятно, государственные меры во время пандемии имели разный эффект в зависимости от тяжести эпидемиологического положения
#включим перекрестные эффекты
mod_FE5<-lm(I(log(credit_rate)) ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate+lwage+debt+lag_debt+lease_delay
            +fin_support+lease_free+tax_decrease+other_tax_free
            +ill_rate*lease_delay+ill_rate*fin_support+ill_rate*lease_free+ill_rate*tax_decrease+ill_rate*other_tax_free
            +factor(region_name)+factor(month), ecm_data)
summary(mod_FE5)

#Значимость перекрестных эффектов
anova(mod_FE4,mod_FE5)
#перекрестные эффекты незначимы

#нестрогая мультиколлинеарность
vif(mod_FE4)
#4 переменные обладают показателями vif > 10, что может вызывать нестрогую мультиколлинеарность:
#lag_ill_rate
#lag_death_rate
#debt
#lag_debt
#это увеличивает ошибки, но оценки всё ещё эффективные и состоятельные

stargazer(mod_FE4, se=list(cse(mod_FE4)),
          keep=c('ill_rate','lag_ill_rate','death_rate','lag_death_rate','lwage','debt','lag_debt', 
                 'lease_delay','fin_support','lease_free','tax_decrease','other_tax_free','lpop','Constant'),
          type='html',
          out="C:/Users/user/Desktop/Работки/Исследование по ковиду/финальная модель.html")

#Воспользуемсяя LASSO, чтобы ранжировать переменные
#Для этого просто построим много моделей с разными параметрами лямбда (чем больше, тем более жестко исключаются переменные)
#Затем смотрим, в каком порядке переменные выходят - чем раньше, тем меньше их объяснительная сила
library('glmnet')
y<-log(ecm_data$credit_rate)
x<-data.matrix(ecm_data[,c('ill_rate','lag_ill_rate','death_rate','lag_death_rate','lwage','debt','lag_debt', 
                           'lease_delay','fin_support','lease_free','tax_decrease','other_tax_free','lpop')])
#lasso
for (lambda in seq(exp(-7),exp(-1.5),length.out=50)) {
  mod <- glmnet(x, y, alpha = 1, lambda = lambda)
  print(coef(mod))
}
"Итого, переменные в LASSO ранжируются в следующем порядке по объяснительной силе
1 - lwage
2 - debt
3 - lag_ill_rate
4 - tax_decrease
5 - other_tax_free
6,7 - lease_free, lag_debt
8 - lpop
9 - death_rate
10 - lease_delay
...
"
#Результат, доход в регионе оказался наиболее важным фактором кредитования ФЛ, более важным, чем заболеваемость


#Посмотрим на гипотезу о снижении влияния заболеваемости со временем
#1 - модели по волнам:
for (wv in 1:4){
  mod<-lm(I(log(credit_rate)) ~ ill_rate+death_rate+lwage+debt+lag_debt+lease_delay
              +fin_support+lease_free+tax_decrease+other_tax_free
              +factor(region_name)+factor(month), ecm_data[ecm_data$wave==wv,])
  nam <- paste("mod", wv, sep = "_")
  assign(nam, mod)
}
stargazer(mod_1,mod_2,mod_3,mod_4,
          keep=c('ill_rate','lag_ill_rate','death_rate','lag_death_rate,','lwage','debt','lag_debt', 
                 'lease_delay','fin_support','lease_free','tax_decrease','other_tax_free','Constant'),
          type='text')

#2 - перекрестные эффекты - добавим произведение бинарной переменной волны на заболеваемость и смертность
mod_hyp3<-lm(I(log(credit_rate)) ~ ill_rate+lag_ill_rate+death_rate+lag_death_rate
             +factor(wave)*ill_rate +factor(wave)*death_rate
             +lwage+debt+lag_debt+lease_delay
             +fin_support+lease_free+tax_decrease+other_tax_free
             +factor(region_name)+factor(month), ecm_data)
stargazer(mod_hyp3, se=list(cse(mod_hyp3)),
          keep=c('ill_rate','lag_ill_rate','death_rate','lag_death_rate,','lwage','debt','lag_debt', 
                 'lease_delay','fin_support','lease_free','tax_decrease','other_tax_free','lpop','Constant'),
          type='text')
#Ни один из перекрестных эффектов не является значимым, значит гипотеза 3 отвергается, в более поздние волны не наблюдается значимого изменения эффекта
