# Set working directory

# load the data
car_price <- read.csv("CarPrice_Assignment.csv")

# check for blank and missing values
length(which(is.na(car_price))) # No NA values found
sapply(car_price, function(x) length(which(x == ""))) # No blank values
sum(duplicated(car_price)) # No duplicate value


library(dplyr)
library(tidyr)
car_price <- separate(car_price, CarName, into=c("CompanyName", "Model"), sep=" ")
car_price <- car_price[,-4]

car_price$CompanyName <- factor(tolower(car_price$CompanyName))

car_price$CompanyName <- factor(gsub("maxda", "mazda", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("vw", "volkswagen", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("vokswagen", "volkswagen", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("porcshce", "porsche", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("toyouta", "toyota", car_price$CompanyName))


# checking the structure of data
str(car_price)

car_price$symboling <- as.factor(car_price$symboling)
levels(car_price$symboling)[1:2] <- "No Risk"
levels(car_price$symboling)[2:3] <- "low risk"
levels(car_price$symboling)[3:4] <- "high risk"

# Checking for Outliers

# checking outliers for wheelbase
quantile(car_price$wheelbase, seq(0,1,0.01))
car_price$wheelbase[which(car_price$wheelbase > 110)] <- 110
boxplot(car_price$wheelbase) # No outliers

# outliers for carwidth
quantile(car_price$carwidth, seq(0,1,0.01))
boxplot(car_price$carwidth) # No outliers

# outliers for carlength
quantile(car_price$carlength, seq(0,1,0.01))
car_price$carlength[which(car_price$carlength < 155.900)] <- 155.900
boxplot(car_price$carlength) 

# outliers for car height
quantile(car_price$carheight, seq(0,1,0.01)) # No outliers
boxplot(car_price$carheight)

# outliers for curbweight
quantile(car_price$curbweight, seq(0,1,0.01))
car_price$curbweight[which(car_price$curbweight > 3768.40)] <- 3768.40
boxplot(car_price$curbweight) 

# outliers for engine size
quantile(car_price$enginesize, seq(0,1,0.01))
car_price$enginesize[which(car_price$enginesize > 194)] <- 194
boxplot(car_price$enginesize) 


# outliers for boreratio
quantile(car_price$boreratio, seq(0,1,0.01))
boxplot(car_price$boreratio) # No outliers

# Outliers for stroke
quantile(car_price$stroke, seq(0,1,0.01))
car_price$stroke[which(car_price$stroke > 3.6400)] <- 3.6400
car_price$stroke[which(car_price$stroke < 2.6400)] <- 2.6400
boxplot(car_price$stroke) # few outliers, can be ignored

# outliers for horsepower
quantile(car_price$horsepower, seq(0,1,0.01))
car_price$horsepower[which(car_price$horsepower > 162.00)] <- 162.00
boxplot(car_price$horsepower) # few outliers, can be ignored

# outliers for peakrpm
quantile(car_price$peakrpm, seq(0,1,0.01))
boxplot(car_price$peakrpm)

# Outliers for citympg
quantile(car_price$citympg, seq(0,1,0.01))
car_price$citympg[which(car_price$citympg > 38)] <- 38
boxplot(car_price$citympg)

# Outliers for highwaympg
quantile(car_price$highwaympg, seq(0,1,0.01))
car_price$highwaympg[which(car_price$highwaympg > 46.92)] <- 46.92
boxplot(car_price$citympg)

# As we can see thre are lots of factor variables in the data. 
#Therefore converting all factors variable into numeric variables is must.

# convert factors with 2 levels to numerical variables
# 2 levels in fuel type i.e. for gas <- 0, for diesal <- 1
levels(car_price$fueltype)<-c(1,0)
car_price$fueltype <- as.numeric(levels(car_price$fueltype))[car_price$fueltype]

# 2 levels in aspiration i.e. for std <- 1, for turbo <- 0
levels(car_price$aspiration)<-c(1,0)
car_price$aspiration <- as.numeric(levels(car_price$aspiration))[car_price$aspiration]

# 2 levels in doornumber i.e. for two <- 0, for four <- 1
levels(car_price$doornumber)<-c(4,2)
car_price$doornumber <- as.numeric(levels(car_price$doornumber))[car_price$doornumber]

# 2 levels for engine location i.e. for front <- 1, for rear <- 0
levels(car_price$enginelocation)<-c(1,0)
car_price$enginelocation <- as.numeric(levels(car_price$enginelocation))[car_price$enginelocation]


# Now we come across variables having 3 or more levels. 
summary(factor(car_price$carbody)) # 5 levels
summary(factor(car_price$drivewheel)) # 3 levels
summary(factor(car_price$enginetype)) # 7 levels
summary(factor(car_price$cylindernumber)) # 7 levels
summary(factor(car_price$fuelsystem)) # 8 levels

#Converting "carbody" into dummy_1 . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = car_price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "convertible". 
dummy_1 <- dummy_1[,-1]

# converting "drivewheel" into dummy_2
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = car_price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "4wd". 
dummy_2 <- dummy_2[,-1]

# converting "enginetype" into dummy_3
dummy_3 <- data.frame(model.matrix( ~enginetype, data = car_price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "dohc". 
dummy_3 <- dummy_3[,-1]

# converting "cylindernumber" into dummy_4
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = car_price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "eight". 
dummy_4 <- dummy_4[,-1]

# converting "fuelsystem" into dummy_5
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = car_price))

#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "1bbl". 
dummy_5 <- dummy_5[,-1]

summary(factor(car_price$CompanyName))
dummy_6 <- data.frame(model.matrix( ~CompanyName, data = car_price))
dummy_6 <- dummy_6[,-1]

dummy_7 <- data.frame(model.matrix( ~symboling, data = car_price))
dummy_7 <- dummy_7[,-1]


# Combine the dummy variables to the main data set, after removing the original categorical "furnishingstatus" column
car_price_1 <- cbind(car_price[,c(-1,-2,-3,-7,-8,-15,-16,-18)], dummy_1, dummy_2, dummy_3, dummy_4, dummy_5, dummy_6, dummy_7)
View(car_price_1)

# Derived Matrices
# city to highway mpg
car_price_1$city_to_highway_mpg <- round(car_price_1$citympg/car_price_1$highwaympg,2)

# bore to stroke ratio
car_price_1$bore_to_stroke_ratio <- round(car_price_1$boreratio /car_price_1$stroke,2)

# horsepower to curbweight
car_price_1$hp_to_curbweight <- round(car_price_1$horsepower/car_price_1$curbweight,2)

# carlength to wheelbase
car_price_1$carlength_to_wheelbase <- round(car_price_1$carlength /car_price_1$wheelbase,2)

write.csv(car_price_1,"car_price_1.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------------------------
  # MODEL BUILDING

set.seed(100)
trainindices= sample(1:nrow(car_price_1), 0.7*nrow(car_price_1))

train = car_price_1[trainindices,]
test = car_price_1[-trainindices,]


# Building model_1 with all variables

model_1 <- lm(price~.,data = train)
summary(model_1)
# Multiple R-squared:  0.979
# Adjusted R-squared:  0.9636
# R-squared value is very high, but very few variables are significant

#using Step AIC to identify insignificant columns 

library(MASS)

step <- stepAIC(model_1, direction="both")
step
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

model_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + boreratio + stroke + 
                compressionratio + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo + bore_to_stroke_ratio, data = train)

summary(model_2)
# Multiple R-squared:  0.9766
# Adjusted R-squared:  0.9683

## Let us check for multicollinearity 
# If the VIF is above 2 or 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
library(car)
sort(vif(model_2))

# As we see bore_to_stroke_ratio has highest VIF = 538.36 and has high p value = 0.208# Next hishest VIF is of highwaympg,it also has p value 0.010 which is near to 0.02, so removing it
model_3 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + boreratio + stroke + 
                compressionratio + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)
summary(model_3)
# Multiple R-squared:  0.9762	
# Adjusted R-squared:  0.9681 
sort(vif(model_3))

# removing fuel type as it has high VIF = 169.70 with  p value  = 0.07
model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + boreratio + stroke + 
                compressionratio + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)
summary(model_4)
# Multiple R-squared:  0.9755	
# Adjusted R-squared:  0.9675 
sort(vif(model_4))

# removing wheel base, VIF = 10.33 and p value = 0.089510
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + boreratio + stroke + 
                compressionratio + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetypel + enginetypeohc + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)
summary(model_5)
# Multiple R-squared:  0.9748	
# Adjusted R-squared:  0.9669
sort(vif(model_5))

# remove enginetypeohc, VIF = 8.88, p value = 0.044
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + boreratio + stroke + 
                compressionratio + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)
summary(model_6)
# Multiple R-squared:  0.9738	
# Adjusted R-squared:  0.9659 
sort(vif(model_6))

# remove carbodyhatchback, VIF = 6.665, p value = 0.117755
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                           carwidth + curbweight + boreratio + stroke + 
                           compressionratio + carbodysedan + carbodywagon + 
                           drivewheelfwd + enginetypel + enginetypeohcf + 
                           enginetyperotor + cylindernumberfive + cylindernumberfour + 
                           cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                           CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                           CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                           CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                           CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                           CompanyNamevolvo, data = train)
summary(model_7)
# Multiple R-squared:  0.9733	
# Adjusted R-squared:  0.9655 
sort(vif(model_7))

# remove boreratio, VIF = 5.79, p value = 0.305078 
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + stroke + 
                compressionratio + carbodysedan + carbodywagon + 
                drivewheelfwd + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)
summary(model_8)
# Multiple R-squared:  0.973,	
# Adjusted R-squared:  0.9655 
sort(vif(model_8))
    
# remove drivewheelfwd, VIF = 5.41, pvalue = 0.170610
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + stroke + 
                compressionratio + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                CompanyNamevolvo, data = train)
summary(model_9)
# Multiple R-squared:  0.9725
# Adjusted R-squared:  0.9652
sort(vif(model_9))

# remove stroke , VIF = 3.72, p value = 0.175568
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + 
                 compressionratio + carbodysedan + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                 CompanyNamevolvo, data = train)
summary(model_10)
# Multiple R-squared:  0.9721,	
# Adjusted R-squared:  0.9649
sort(vif(model_10))



# As curbwidth and carwidth is highly corelated as correlation = 0.88, so removing one of them

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + 
                 compressionratio + carbodysedan + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen + 
                 CompanyNamevolvo, data = train)
summary(model_11)
# Multiple R-squared:  0.9651,	
# Adjusted R-squared:  0.9565
sort(vif(model_11))

# removing companynamevolvo, VIF = 2.77, p value = 0.135784
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + 
                 compressionratio + carbodysedan + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_12)
# Multiple R-squared:  0.9644	
# Adjusted R-squared:  0.9561
sort(vif(model_12))

# removing carbodysedan , pvalue = 0.903959
model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + 
                 compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNamesaab + CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_13)
# Multiple R-squared:  0.9644	
# Adjusted R-squared:  0.9564
sort(vif(model_13))

# remove CompanyNamesaab pvalue =0.284769

model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + 
                 compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_14)
# Multiple R-squared:  0.9641	
# Adjusted R-squared:  0.9564
sort(vif(model_14))

# removing CompanyNamemercury, pvalue = 0.293215 
model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + 
                 compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_15)
# Multiple R-squared:  0.9637	
# Adjusted R-squared:  0.9563
sort(vif(model_15))

# removing compressionratio  pvalue = 0.330068 

model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_16)
# Multiple R-squared:  0.9634	
# Adjusted R-squared:  0.9463

# remove aspiration pvalue = 0.210884

model_17 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_17)
# Multiple R-squared:  0.9629	
# Adjusted R-squared:  0.9561

# remove CompanyNamehonda, pvalue =0.010395 *

model_18 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_18)
# Multiple R-squared:  0.9608	
# Adjusted R-squared:  0.954

# remove CompanyNamenissan = 0.098849  

model_19 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemazda + CompanyNamemitsubishi + 
                 CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_19)
# Multiple R-squared:  0.9599	
# Adjusted R-squared:  0.9534

# remove CompanyNamemazda = 0.049885 *

model_20 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemitsubishi + CompanyNameplymouth + CompanyNamerenault + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_20)
# Multiple R-squared:  0.9587	
# Adjusted R-squared:  0.9523

# CompanyNamerenault = 0.060082

model_21 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemitsubishi + CompanyNameplymouth + 
                 CompanyNametoyota + CompanyNamevolkswagen, data = train)
summary(model_21)
# Multiple R-squared:  0.9574	
# Adjusted R-squared:  0.9513

# CompanyNamevolkswagen =0.075677

model_22 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemitsubishi + CompanyNameplymouth + 
                 CompanyNametoyota, data = train)
summary(model_22)
# Multiple R-squared:  0.9563	
# Adjusted R-squared:  0.9504

# remove CompanyNameplymouth = 0.079761 

model_23 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemitsubishi + 
                 CompanyNametoyota, data = train)
summary(model_23)
# Multiple R-squared:  0.9552	
# Adjusted R-squared:  0.9496

# remove CompanyNamedodge = 0.141840 

model_24 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemitsubishi + CompanyNametoyota, data = train)
summary(model_24)
# Multiple R-squared:  0.9545	
# Adjusted R-squared:  0.9491

# remove enginetypeohcf = 0.137072 

model_25 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNameisuzu + CompanyNamejaguar + 
                 CompanyNamemitsubishi + CompanyNametoyota, data = train)

summary(model_25)
# Multiple R-squared:  0.9537\	
# Adjusted R-squared:  0.9486

# remove CompanyNameisuzu = 0.044939 *

model_26 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNamejaguar + CompanyNamemitsubishi + CompanyNametoyota, data = train)
summary(model_26)
# Multiple R-squared:  0.9522	
# Adjusted R-squared:  0.9474

# remove CompanyNamemitsubishi = 0.011521 *

model_27 <- lm(formula = price ~ enginelocation + 
                 curbweight + carbodywagon + enginetypel + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNamejaguar + CompanyNametoyota, data = train)

summary(model_27)
# Multiple R-squared:  0.9498	
# Adjusted R-squared:  0.9451

Predict_1 <- predict(model_27,test[,-18])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
# r squared comes out to be 0.85































