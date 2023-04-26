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

-----------------------------------------
#Работа с таблицей new_df, преобработка
  
# Новая таблица с столбцами которіе потенциально подходят
new_df <- subset(df, select =  c(regio1, regio2, baseRentRange, geo_plz , serviceCharge, yearConstructed, heatingType, telekomTvOffer, newlyConst, balcony, picturecount, pricetrend, totalRent, scoutId, noParkSpaces , firingTypes, hasKitchen, cellar, houseNumber, livingSpace, geo_krs, condition, petsAllowed, lift, typeOfFlat, noRooms, floor, numberOfFloors, noRoomsRange, garden, heatingCosts, energyEfficiencyClass, lastRefurbish, date))
View(new_df)


# общее количество пропусков по колонкам
sapply(new_df, function (x) sum(is.na (x)))
# тільки стовпчики в яких є Na
new_df %>% summarise(across(everything(), ~ sum(is.na(.)))) %>%
  select(where(~ all(.) > 0))
# частка пропущенних значень в стовпчиках в яких є НА
new_df %>% summarise(across(everything(), ~ mean(is.na(.)))) %>%
  select(where(~ all(.) > 0))


# чистим данные (delete NA)
new_df <- new_df[complete.cases(new_df$totalRent), ]


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


# перетворюємо змінні char в factor
new_df <- new_df %>% mutate(hasKitchen = as.factor(hasKitchen), heatingType  = as.factor(heatingType ),
                            firingTypes  = as.factor(firingTypes ), typeOfFlat  = as.factor(typeOfFlat ))

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



----------------------------
# дослідження totalRent, бо це цільова функція  

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


----------------------
# usefull things
  
# count_values (for char or factor)
table(new_df$typeOfFlat)












