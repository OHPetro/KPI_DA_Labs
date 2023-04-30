library(psych) 
library(ggplot2) 
library(dplyr)


#  дата считваем 
df <- read.csv("")

--------------------------------------
#Первий взгляд на изначальную таблицу df
  
#общее количество пропусков 
sum(is.na(df))

# общее сведения
str(df)
summary(df)

# количество пропусков по колонкам
sapply(df, function (x) sum(is.na (x)))








-------------------------------------------------------
#Работа с таблицей new_df, преобработка, удаления данных
--------------------------------------------------------
  
# Новая таблица с столбцами которые потенциально подходят
new_df <- subset(df, select =  c(regio1, regio2, baseRentRange, geo_plz , serviceCharge, yearConstructed, heatingType, telekomTvOffer, newlyConst, balcony, picturecount, pricetrend, totalRent , firingTypes, hasKitchen, cellar, livingSpace, geo_krs, condition, petsAllowed, lift, typeOfFlat, noRooms, floor, noRoomsRange, garden, date))
View(new_df)

---------------------
# Обработка пропусків

# обзор пропусков 
# общее количество пропусков по колонкам
sapply(new_df, function (x) sum(is.na (x)))
# тільки стовпчики в яких є Na
new_df %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
  select(where(~ all(.) > 0))
# частка пропущенних значень в стовпчиках в яких є НА
new_df %>% summarise(across(everything(), ~ mean(is.na(.)))) %>%
  select(where(~ all(.) > 0))

# чистим пропуски
# видалення стрік в таблиці по заданим колонкам де є НА
new_df <- new_df[complete.cases(new_df$totalRent, new_df$typeOfFlat,new_df$pricetrend,new_df$condition,new_df$firingTypes ), ]
# замна середнім
new_df$serviceCharge <- ifelse(is.na(new_df$serviceCharge), mean(new_df$serviceCharge, na.rm = TRUE), new_df$serviceCharge)
# заміна найбільш часто зустрічающимся елементом 
most_common <- names(which.max(table(new_df$telekomTvOffer)))
new_df$telekomTvOffer <- ifelse(is.na(new_df$telekomTvOffer), most_common, new_df$telekomTvOffer)

most_common <- names(which.max(table(new_df$heatingType)))
new_df$telekomTvOffer <- ifelse(is.na(new_df$heatingType), most_common, new_df$heatingType)
# встановлення флагів
new_df$petsAllowed <- ifelse(is.na(new_df$petsAllowed), "no information", new_df$petsAllowed)
new_df$yearConstructed <- ifelse(is.na(new_df$yearConstructed), -1, new_df$yearConstructed)

# Обработка пропусків\
---------------------
  
---
# невеликий розвідувальний аналіз фітч для розуміння , як боротися з НА
# вивід 
new_df %>% summarise(across(everything(), ~ mean(is.na(.)))) %>%
  select(where(~ all(.) > 0))

table(new_df$floor)
table(new_df$heatingType)
table(new_df$petsAllowed)
table(new_df$telekomTvOffer)

table(new_df$heatingType)
ggplot(new_df, aes(x = heatingType)) + 
  geom_bar(binwidth = 1, alpha = 0.5, aes(fill = heatingType)) +
  facet_wrap(~typeOfFlat, nrow = 1)

ggplot(new_df, aes(x = heatingType)) +
  geom_bar(binwidth = 0.5, position = "dodge") +
  labs(x = "Значение", y = "Частота", fill = "Группа") +
  theme_minimal()


ggplot(new_df, aes(x = firingTypes)) +
  geom_bar(binwidth = 0.5, position = "dodge") +
  labs(x = "Значение", y = "Частота", fill = "Группа") +
  theme_minimal()

cor(new_df$totalRent,new_df$numberOfFloors, use="pairwise.complete.obs")

#heating costs
# Определяем выбросы
q1 <- quantile(new_df$heatingCosts, 0.25, na.rm = TRUE)
q3 <- quantile(new_df$heatingCosts, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr
outliers <- which(new_df$heatingCosts < lower | new_df$heatingCosts > upper)
# Удаляем строки с выбросами
new_df <- new_df[-outliers, ]
# Построение графика выбросов heating costs
bx <- boxplot.stats(new_df$serviceCharge)$out
plot(bx, pch=19, col="red", main="Выбросы")
hist(new_df$serviceCharge, breaks = 30, col = "blue", xlab = "Значения", main = "Гистограмма")
#corr
cor(new_df$serviceCharge, new_df$heatingCosts, use="pairwise.complete.obs")

---

---
# Заміна типів в колнках 
# перетворюємо змінні char в factor
new_df <- new_df %>% mutate(hasKitchen = as.factor(hasKitchen), heatingType  = as.factor(heatingType ),
                            firingTypes  = as.factor(firingTypes ), typeOfFlat  = as.factor(typeOfFlat ) )

df$column <- as.integer(df$column)
---
  
---
# Правільність вводу данних


-------------------------------------------------------
#Работа с таблицей new_df, преобработка, удаления данных\
--------------------------------------------------------
  
  
  
  


  
  

  


---------------------------------------------------------------
  #E
  #D
  #A
# любие метрики totalRent должны быть рассмотрени с точки зрения typeOfFloat
---------------------------------------------------------------
# общее сведения
str(new_df)
#summary(new_df %>% select_if(is.integer))
summary(new_df %>% select_if(is.numeric))
# summary++ (sd)
bind_rows(
  mean = new_df %>% summarize(across(where(is.numeric), mean, na.rm = TRUE)),
  sd = new_df %>% summarize(across(where(is.numeric), sd, na.rm = TRUE)),
  .id = "statistic"
)



--------------------------------------
# Дослідження totalRent, цільова фітча  

# общее
summary(new_df$totalRent)
sum(new_df$my_column == 0)
  
# определение выбросов
q1 <- quantile(new_df$totalRent, 0.25, na.rm= TRUE)
q3 <- quantile(new_df$totalRent, 0.75, na.rm= TRUE)
iqr <- q3 - q1
lower <- q1 - 1.5*iqr
upper <- q3 + 1.5*iqr
outliers <- new_df$totalRent[new_df$totalRent < lower | new_df$totalRent > upper]  
length(outliers)

#boxplot количества выкидов по категории typeOfFlat. totalRent логарифмирован 
ggplot(new_df, aes(x = typeOfFlat, y = log(totalRent))) +
  geom_boxplot(aes(fill=names(new_df$typeOfFlat))) +
  labs(x = "typeOfFlat", y = "totalRent") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))


# процент выкидов в totalRent по каждой категории 
new_df %>%
  group_by(typeOfFlat) %>%
  summarize(percent_outliers = mean(totalRent < lower | totalRent > upper, na.rm = TRUE) * 100)

# Дослідження totalRent, цільова фітча\
--------------------------------------
  
--------------------------------------
# Дослідження typeOfFloat, інша важлива фітча
  
#
table(new_df$typeOfFlat)

# bar plot typeOfFlat
ggplot(new_df, aes(x = typeOfFlat)) +
  geom_bar() +
  labs(x = "typeOfFlat", y = "Counts") +
  scale_x_discrete(labels = names(table(new_df$typeOfFlat))) 

# Дослідження typeOfFloat, інша важлива фітча\
--------------------------------------

-------------------------------------
# ВИкиди
  
# Создание функции для определения выбросов
outliers <- function(x, na.rm = TRUE) {
  q1 <- quantile(x, probs = c(0.25), na.rm = na.rm)
  q3 <- quantile(x, probs = c(0.75), na.rm = na.rm)
  iqr <- q3 - q1
  up <- q3 + 1.5 * iqr
  down <- q1 - 1.5 * iqr
  sum(x > up | x < down, na.rm = na.rm)
}
# Фильтрация столбцов с типом numeric
numeric_cols <- new_df %>% 
select_if(is.numeric) 
# Применение функции outliers для каждого столбца
outliers_count <- sapply(numeric_cols, outliers)
# Вывод количества выбросов для каждого столбца
outliers_count
# Бачимо зо є викиди в serviceCharge,livingSpace


---
#livingSpace
#boxplot количества выкидов по категории typeOfFlat. totalRent логарифмирован 
ggplot(new_df, aes(x = typeOfFlat, y = log(livingSpace))) +
geom_boxplot(aes(fill=names(new_df$typeOfFlat))) +
labs(x = "typeOfFlat", y = "livingSpace") +
theme(axis.title = element_text(size = 25),
      axis.text = element_text(size = 20))
#процент выкидов в livingSpace по каждой категории 
new_df %>%
  group_by(typeOfFlat) %>%
  summarize(percent_outliers = mean(livingSpace < lower | livingSpace > upper, na.rm = TRUE) * 100)

summary(new_df$livingSpace)
table(new_df$livingSpace)
# побачили що жил плозща =0,1 це не можливо треба викинути і ціна влеика за цю жил площу
new_df[new_df$livingSpace == 8,]

new_df <- new_df[new_df$livingSpace != 0,]
new_df <- new_df[new_df$livingSpace != 1,]
----

---
#serviceCharge
#boxplot количества выкидов по категории typeOfFlat. totalRent логарифмирован 
ggplot(new_df, aes(x = typeOfFlat, y = log(serviceCharge))) +
geom_boxplot(aes(fill=names(new_df$typeOfFlat))) +
labs(x = "typeOfFlat", y = "serviceCharge") +
theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
#процент выкидов в serviceCharge по каждой категории 
new_df %>%
  group_by(typeOfFlat) %>%
  summarize(percent_outliers = mean(serviceCharge < lower | serviceCharge > upper, na.rm = TRUE) * 100)

summary(new_df$serviceCharge)
table(new_df$serviceCharge)

# видаляємо ті строки де цна аренди більше ніж плата за комунальні послуги(боримося з викидами)
new_df <- new_df[new_df$serviceCharge <= new_df$totalRent, ]
---

# ВИкиди \
-------------------------------------



# бокс плто всех столбиков с  нум котировкой сгрупированих по ТАйп флоат. 
#табица всех вібросов по тайп флоат 



---------------------------------------------------------------
#E
#D
#A
# любие метрики totalRent должны быть рассмотрени с точки зрения typeOfFloat\
---------------------------------------------------------------  
  
  
  

  
  
  
  








--------------------------------------------------------
  
  # just examples of plots  
  
  # bar plot
  ggplot(new_df, aes(x = typeOfFlat)) +
  geom_bar() +
  labs(x = "typeOfFlat", y = "Counts") +
  scale_x_discrete(labels = names(table(new_df$typeOfFlat))) 



## Graphicks
ggplot(new_df, aes(x = noRooms, y = heatingCosts, color = balcony)) +
  geom_point() +
  labs(x = "noRooms", y = "heatingCosts",
       title = "Plot", color = "balcony") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Жінка", "Чоловік")) 



# dva grafika
ggplot(passengers, aes(x = Age, y = Fare)) +
  geom_point() +
  labs(x = "Вік пасажира", y = "Ціна за квиток, фунти") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        strip.text = element_text(size = 20)) +
  facet_wrap(~Sex, labeller = as_labeller(c("female" = "Жінка", "male" = "Чоловік")))

# boxplot
ggplot(new_df, aes(x = typeOfFlat, y = log(totalRent))) +
  geom_boxplot(aes(fill=names(new_df$typeOfFlat))) +
  labs(x = "typeOfFlat", y = "totalRent") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))
