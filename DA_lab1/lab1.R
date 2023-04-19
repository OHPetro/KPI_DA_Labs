library(psych)

sum(is.na(df))
sum(is.na(df$totalRent))
sum(is.na(df$heatingCosts))


str(df)
summary(df)

new_df <- subset(df, select =  c(regio1, regio2, baseRentRange, geo_plz , serviceCharge, yearConstructed, heatingType, telekomTvOffer, newlyConst, balcony, picturecount, pricetrend, totalRent, scoutId, noParkSpaces , firingTypes, hasKitchen, cellar, houseNumber, livingSpace, geo_krs, condition, petsAllowed, lift, typeOfFlat, noRooms, floor, numberOfFloors, noRoomsRange, garden, heatingCosts, energyEfficiencyClass, lastRefurbish, date))
View(new_df)

str(new_df)
summary(new_df)

sapply(new_df, function (x) sum(is.na (x)))