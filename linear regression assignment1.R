car_price <- read.csv("CarPrice_Assignment.csv")

# check for blank and missing values as a part of data cleaning
length(which(is.na(car_price))) # No NA values found
sapply(car_price, function(x) length(which(x == ""))) # No blank values
sum(duplicated(car_price)) # No duplicate value


library(dplyr)
library(tidyr)
# seperate CarName column variable into CompanyName and Model
car_price <- separate(car_price, CarName, into=c("CompanyName", "Model"), sep=" ")
# removing Model variable as it is not necessary in analysis
car_price <- car_price[,-4]

# doing company name variable in lowercase
car_price$CompanyName <- factor(tolower(car_price$CompanyName))

# as there are spelling mistakes in companyname variable, so correcting it as a part of data cleaning.
car_price$CompanyName <- factor(gsub("maxda", "mazda", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("vw", "volkswagen", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("vokswagen", "volkswagen", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("porcshce", "porsche", car_price$CompanyName))
car_price$CompanyName <- factor(gsub("toyouta", "toyota", car_price$CompanyName))


# checking the structure of data
str(car_price)

# making symboling vaiable in No Risk, low risk and high risk
car_price$symboling <- as.factor(car_price$symboling)
levels(car_price$symboling)[1:2] <- "No Risk"
levels(car_price$symboling)[2:3] <- "low risk"
levels(car_price$symboling)[3:4] <- "high risk"

# Checking for Outliers

# checking outliers for wheelbase
quantile(car_price$wheelbase, seq(0,1,0.01))
car_price$wheelbase[which(car_price$wheelbase > 110)] <- 110
boxplot(car_price$wheelbase)

# outliers for carwidth
quantile(car_price$carwidth, seq(0,1,0.01))
boxplot(car_price$carwidth) 

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
boxplot(car_price$boreratio)

# Outliers for stroke
quantile(car_price$stroke, seq(0,1,0.01))
car_price$stroke[which(car_price$stroke > 3.6400)] <- 3.6400
car_price$stroke[which(car_price$stroke < 2.6400)] <- 2.6400
boxplot(car_price$stroke) 

# outliers for horsepower
quantile(car_price$horsepower, seq(0,1,0.01))
car_price$horsepower[which(car_price$horsepower > 162.00)] <- 162.00
boxplot(car_price$horsepower)

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

# 2 levels in doornumber i.e. for two <- 2, for four <- 4
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

# horsepower to curbweight
car_price_1$hp_to_curbweight <- round(car_price_1$horsepower/car_price_1$curbweight,2)

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
# Multiple R-squared:  0.9785
# Adjusted R-squared:  0.9636
# R-squared value is very high, but very few variables are significant

#using Step AIC to identify insignificant columns 

library(MASS)

step <- stepAIC(model_1, direction="both")
step
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

model_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + compressionratio + horsepower + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk + symbolinghigh.risk, 
              data = train)

summary(model_2)
# Multiple R-squared:  0.9765
# Adjusted R-squared:  0.9688

## Let us check for multicollinearity 
# If the VIF is above 2 or 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
library(car)
sort(vif(model_2))

# As we see fuel type has highest VIF = 161.49 and has high p value = 0.077150, removing it
model_3 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + compressionratio + horsepower + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk + symbolinghigh.risk, 
              data = train)
summary(model_3)
# Multiple R-squared:  0.9758	
# Adjusted R-squared:  0.9682 
sort(vif(model_3))

# removing horsepower as it has high VIF = 16.92 with  p value  = 0.214342
model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + compressionratio + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk + symbolinghigh.risk, 
              data = train)
summary(model_4)
# Multiple R-squared:  0.9755	
# Adjusted R-squared:  0.968 
sort(vif(model_4))

# removing wheel base, VIF = 12.16 and p value = 0.019861
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + compressionratio + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk + symbolinghigh.risk, 
              data = train)
summary(model_5)
# Multiple R-squared:  0.9742	
# Adjusted R-squared:  0.9667
sort(vif(model_5))

# remove carbodysedan, VIF = 13.916, p value = 0.047049
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + compressionratio + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk + symbolinghigh.risk, 
              data = train)
summary(model_6)
# Multiple R-squared:  0.9733	
# Adjusted R-squared:  0.9658 
sort(vif(model_6))

# remove enginetypeohc, VIF = 6.26, p value = 0.026442
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + compressionratio + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                enginetypel + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk + symbolinghigh.risk, 
              data = train)
summary(model_7)
# Multiple R-squared:  0.972	
# Adjusted R-squared:  0.9646 
sort(vif(model_7))

# remove symbolinghighrisk, VIF = 4.58, p value = 0.245909 
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + compressionratio + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                enginetypel + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_8)
# Multiple R-squared:  0.9717	
# Adjusted R-squared:  0.9644 
sort(vif(model_8))

# As curbweight and carwidth are highly corelated = 0.88, removing carwidth as it is less significant than curbweight
# remove drivewheelfwd, VIF = 5.41, pvalue = 0.170610
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                curbweight + compressionratio + 
                carbodyhardtop + carbodyhatchback + carbodywagon + 
                enginetypel + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_9)
# Multiple R-squared:  0.9652
# Adjusted R-squared:  0.9567
sort(vif(model_9))
#-----------------------------------------------------------------------------------
# As cylindernumberfour has high VIF, but it is highly significant, so lets remove and see the effect.
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + compressionratio + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + cylindernumbersix + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_10)
# Multiple R-squared:  0.9455,	
# Adjusted R-squared:  0.9327
sort(vif(model_10))
# as there is not much of difference in the adjusted R square, so this model is accepted.


# remove companynamesix , VIF = 2.79, pvalue = 0.188067

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + compressionratio + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_11)
# Multiple R-squared:  0.9446,	
# Adjusted R-squared:  0.9322
sort(vif(model_11))

# Now removing variable based on p value.
# removing carbodyhardtop, pvalue = 0.808851
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 curbweight + compressionratio + carbodyhatchback + carbodywagon + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_12)
# Multiple R-squared:  0.9446	
# Adjusted R-squared:  0.9328
sort(vif(model_12))

# removing aspiration, pvalue = 0.566791
model_13 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodyhatchback + carbodywagon + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_13)
# Multiple R-squared:  0.9444	
# Adjusted R-squared:  0.9331
sort(vif(model_13))

# remove carbodyhatchback pvalue = 0.241258 

model_14 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_14)
# Multiple R-squared:  0.9438	
# Adjusted R-squared:  0.9329
sort(vif(model_14))

# removing CompanyNamemercury pvalue =  0.112585
model_15 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_15)
# Multiple R-squared:  0.9426	
# Adjusted R-squared:  0.9321
sort(vif(model_15))

# removing enginetyperotor pvalue = 0.088992

model_16 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen + symbolinglow.risk, data = train)
summary(model_16)
# Multiple R-squared:  0.9412	
# Adjusted R-squared:  0.931

# remove symbolinglow.risk pvalue = 0.143288 

model_17 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota + 
                 CompanyNamevolkswagen, data = train)
summary(model_17)
# Multiple R-squared:  0.9401	
# Adjusted R-squared:  0.9303

# remove CompanyNamevolkswagen pvalue = 0.010324 *

model_18 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + 
                 CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota, data = train)
summary(model_18)
# Multiple R-squared:  0.9368	
# Adjusted R-squared:  0.927

# remove CompanyNamenissan  pvalue = 0.033686 * 

model_19 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + CompanyNamehonda + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + CompanyNamemitsubishi +  
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota, data = train)
summary(model_19)
# Multiple R-squared:  0.9344	
# Adjusted R-squared:  0.9249

# remove CompanyNamehonda  pvalue = 0.137420

model_20 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemazda + CompanyNamemitsubishi +  
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota, data = train)
summary(model_20)
# Multiple R-squared:  0.9332	
# Adjusted R-squared:  0.9242

# remove CompanyNamemazda pvalue = 0.121368

model_21 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNameplymouth + CompanyNamerenault + CompanyNametoyota, data = train)
summary(model_21)
# Multiple R-squared:  0.9319	
# Adjusted R-squared:  0.9233

# remove CompanyNamerenault  pvalue = 0.096792

model_22 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 cylindernumberfive + CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNameplymouth + CompanyNametoyota, data = train)
summary(model_22)
# Multiple R-squared:  0.9304	
# Adjusted R-squared:  0.9222

# remove cylindernumberfive  pvalue = 0.074455 

model_23 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamedodge + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNameplymouth + CompanyNametoyota, data = train)
summary(model_23)
# Multiple R-squared:  0.9287	
# Adjusted R-squared:  0.9209

# remove CompanyNamedodge  pvalue = 0.081561

model_24 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + enginetypeohcf + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNameplymouth + CompanyNametoyota, data = train)
summary(model_24)
# Multiple R-squared:  0.9269	
# Adjusted R-squared:  0.9196

# remove enginetypeohcf  pvalue = 0.104765  

model_25 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNameplymouth + CompanyNametoyota, data = train)

summary(model_25)
# Multiple R-squared:  0.9254	
# Adjusted R-squared:  0.9185

# remove CompanyNameplymouth pvalue = 0.105953

model_26 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNameisuzu + CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNametoyota, data = train)
summary(model_26)
# Multiple R-squared:  0.9239	
# Adjusted R-squared:  0.9175

# remove CompanyNameisuzu pvalue = 0.066344

model_27 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 enginetypel + CompanyNamebmw + CompanyNamebuick + 
                 CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNametoyota, data = train)

summary(model_27)
# Multiple R-squared:  0.9219	
# Adjusted R-squared:  0.916

# remove enginetypel  pvalue = 0.022694
model_28 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamejaguar + CompanyNamemitsubishi +  
                 CompanyNametoyota, data = train)

summary(model_28)
# Multiple R-squared:  0.9188	
# Adjusted R-squared:  0.9133

# remove CompanyNamemitsubishi pvalue = 0.009425
model_29 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamejaguar +  
                 CompanyNametoyota, data = train)

summary(model_29)
# Multiple R-squared:  0.9145	
# Adjusted R-squared:  0.9094

# remove CompanyNametoyota pvalue = 0.012696
model_30 <- lm(formula = price ~ enginelocation + curbweight + compressionratio + carbodywagon + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamejaguar, data = train)

summary(model_30)
# Multiple R-squared:  0.9105	
# Adjusted R-squared:  0.9058

Predict_1 <- predict(model_30,test[,-18])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
# r squared comes out to be 0.85

#r squared from test dataset is 0.85 and traning data is 0.905. The difference is approx 5%, which is
#reasonably an accurate model

#The final adjusted r square and the r square are similar.
#R-squared:  0.9105,	Adjusted R-squared:  0.9058

#---------------------------------------------------------------------

#Below are the variables that are part of the final model

#1.enginelocation
#2.curbweight
#3.compression ratio
#4.Carbodywagon
#5.CompanyNamebmw
#6.CompanyNamebuick
#7.CompanynameJaguar



############################################## Conclusions ###########################################


# Model_30 predicts car price with sufficent accuracy, contains only highly significant and has little
# to no multicollinearity


# Key variables used for car price prediction

# 1. Engine location - price varies with location of engine - rear/front
# 2. car brand - bmw, buick and jaguar have significantly higher prices than other cars
# 3. curbweight, carbody and compression ratio are other variables on which price of cars are dependent

# For the negatively dependent variables, increase in value would decrease price for cars

#Depending on these variables/parameters the Geely Auto manufacturers can develop appropriate car models with proper pricing.





























