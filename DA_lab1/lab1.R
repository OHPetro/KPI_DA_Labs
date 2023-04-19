library(psych) 

#  дата считваем 
df <- read.csv("")

#общее количество пропусков 
sum(is.na(df))

# общее сведения
str(df)
summary(df)

# делаем новую таблицу с теми столбцами что нам подходят
new_df <- subset(df, select =  c(regio1, regio2, baseRentRange, geo_plz , serviceCharge, yearConstructed, heatingType, telekomTvOffer, newlyConst, balcony, picturecount, pricetrend, totalRent, scoutId, noParkSpaces , firingTypes, hasKitchen, cellar, houseNumber, livingSpace, geo_krs, condition, petsAllowed, lift, typeOfFlat, noRooms, floor, numberOfFloors, noRoomsRange, garden, heatingCosts, energyEfficiencyClass, lastRefurbish, date))
View(new_df)

# общее сведения
str(new_df)
summary(new_df)

# общее количество пропусков по колонкам
sapply(new_df, function (x) sum(is.na (x)))




