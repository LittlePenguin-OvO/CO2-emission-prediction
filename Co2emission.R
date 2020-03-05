###################################################################################
###################################################################################
####                                                                           ####
####                  Analysis of Emissionsdata                                ####
####                                                                           ####
####    This script mainly concentrates on understanding the variation of CO2  ####
####    emissions from different countries, different manufactuers and         ####
####    different years. Then try to predict some obervations with missing     ####
####    CO2 emissions.                                                         ####
####                                                                           ####
###################################################################################

###################################################################################
###                                                                             ###
###             Part I: Preprocessing the data                                  ###
###                                                                             ###
###   In this part, I will do some preprocessing work in order to do a better   ###
###   exploratory analysis.                                                     ###
###                                                                             ###
###################################################################################
library(plyr)
#
# Import the data.
#
InputLines <- scan("EmissionsData.csv", what=character(), sep="\n")
InputLines <- iconv(InputLines, to="ASCII", sub="@")
write(InputLines, file="TranslatedData.csv")
EmissionsData <- read.csv("TranslatedData.csv", na.strings="-1")

#
# To find the first look of data structres. 
# We can find some problems here: 
#
#   1. Some Fueltypes and ITchnoltype have not been entered consistently here. 
#   2. Some car models are reported to many different countries, which may create
#      duplicated values.
#   3. Year is regard as numerical variable.

str(EmissionsData)

#
# Rename the levels of Fueltype factor.
#

EmissionsData$FuelType <- toupper(EmissionsData$FuelType)
EmissionsData$FuelType <- as.factor(EmissionsData$FuelType)

# 
# Rename the levels of MfrGroup.
# 

EmissionsData$MfrGroup <- toupper(EmissionsData$MfrGroup)
EmissionsData$MfrGroup <- as.factor(EmissionsData$MfrGroup)
EmissionsData$MfrGroup <- mapvalues(EmissionsData$MfrGroup, from = c("TATA MOTORS LTD; JAGUAR CARS LTD ; LAND ROVER", "TOYOTA -DAIHATSU GROUP",
                                                                     "POOL RENAULT"), to = c("TATA MOTORS LTD; JAGUAR CARS LTD; LAND ROVER", 
                                                                                             "TOYOTA-DAIHATSU GROUP", "RENAULT"))
EmissionsData$MfrGroup <- as.factor(EmissionsData$MfrGroup)

#
# define a dataset that CO2 is not a missing value.
#

FittingData <- EmissionsData[!is.na(EmissionsData$CO2), ]

#
#  Now make a character representation of each row excluding
#  ID, Year, MemberState, MfrGroup, MfrHarmonised, ApprovalNo, Make, CommercialName and Registrations.
#

DataAsChars <-
  apply(FittingData[,-c(1,2,3,5,6,7,8,9)], MARGIN=1, FUN=paste, collapse=" ")

#
#   Calculate the frequency of occurence of each combination
#

DupTable <- as.data.frame(table(DataAsChars))

#
#   Sort the data in a same order. 
#   Then replicate the weights for each duplicated vehicle
#   the appropriate number of times.
#

FittingData <- FittingData[order(DataAsChars),]
DupTable <- DupTable[order(DupTable$DataAsChars),]
FittingData$Weights <- rep(1/DupTable$Freq, DupTable$Freq)

#
#   Put everything back in the original order, copy the weights back into the original
#   dataset and clean up
#

FittingData <- FittingData[order(FittingData$ID),]
EmissionsData$Weights <- NA
EmissionsData[!is.na(EmissionsData$CO2),] <- FittingData
rm(DupTable)

# convert Year to categorical variables.

EmissionsData$Year <- as.factor(EmissionsData$Year)

# change all missing values in ITReduction to 0.

EmissionsData$ITReduction[is.na(EmissionsData$ITReduction)] <- 0

table(EmissionsData$ITReduction)
###################################################################################
##                                                                               ##
##                    Part II: Exploratory anaylsis                              ##
##                                                                               ##
##      This part, we will do some exploratory anaylsis work.                    ##
##      What I do is as following:                                               ##
##        1. Look at the summary of numerical variables and response variables.  ##
##        2. Use PCA to reduce dimensions.                                       ##
##        3. Look at relationship between CO2 and categorical variables.         ##
##        4. Check the assumption for linear model.                              ##
##                                                                               ##
###################################################################################



###################################################################################
##                                                                               ##
##       1. Find the summary of numerical variables and response variable        ##
##                                                                               ##
###################################################################################

# Look at the useful summary.

summary(EmissionsData[,c(10,11,12,13,14,17,18)])

# Check observations that CO2 emissions are zero.

zero_emissions <- EmissionsData[which(EmissionsData$CO2 == 0),]

#
# We can see that only cars using Electric as fuel have 0 CO2 emissions.
# But does every electric car have 0 CO2 emissions? The answer is "NO".
# Note: There is a strange observation : ID 1131. All other electric cars have
# 0 engine size, and its power is significanly higher than other electric cars. 
# So I suggest that remove this observation.
#

electric_car <- EmissionsData[which(EmissionsData$FuelType == "ELECTRIC"),]

# Remove the outlier.

EmissionsData <- EmissionsData[which(EmissionsData$ID != 1131),]

#####################################################################
##                                                                 ##
##              2. Dimension reduction: PCA                        ##
##                                                                 ##
##   Here, we want to reduce dimensions by PCA.                    ##
##   Finally, we reduce from 6 variables to 2 variables.           ##
##                                                                 ##
##                                                                 ##
#####################################################################

# Find the correlation matrix of numeric variables in EmissionsData.
# We can find that EngineSize and Power are highly correlated, but they
# seem not related to another 4 variables. 

NumVars <- c(11,12,13,14,17,18)

round(cor(EmissionsData[,NumVars]),2)  

# Find the principle component for first group.

PCs_first <- prcomp(EmissionsData[,(11:14)], scale=TRUE, retx=TRUE) 

# PC1 and PC2 accounts for 92% of total variation. So we can use these to replace original variables. 

summary(PCs_first)  

pca_coef1 <- round(PCs_first$rotation,3)

EmissionsData$PC1 <- EmissionsData[,11]*pca_coef1[1,1]+EmissionsData[,12]*pca_coef1[2,1] + 
  EmissionsData[,13]*pca_coef1[3,1]+EmissionsData[,14]*pca_coef1[4,1]

EmissionsData$PC2 <- EmissionsData[,11]*pca_coef1[1,2]+EmissionsData[,12]*pca_coef1[2,2] + 
  EmissionsData[,13]*pca_coef1[3,2]+EmissionsData[,14]*pca_coef1[4,2]

# PCA for second group.

PCs_second <- prcomp(EmissionsData[,(17:18)], scale=TRUE, retx=TRUE) 

summary(PCs_second) 

# PC1 accounts for 95% of total variation. So we can use these to replace original variables. 

pca_coef2 <- round(PCs_second$rotation,3)

EmissionsData$PC3 <- EmissionsData[,17]*pca_coef2[1,1] + EmissionsData[,18]*pca_coef2[2,1]


###################################################################################
##                                                                               ##
##         3. Find the relation between CO2 and categorical variables.           ##
##                                                                               ##
##                                                                               ##
###################################################################################

par(mfrow = c(1,1))
# Plot the boxplot to find the relationship.

plot(EmissionsData$FuelType, EmissionsData$CO2, xlab = "Fuel Types", ylab = "CO2 emissions",
     main = "Plot 2.1: Relationship Between Fuel Types and CO2", cex.axis=0.8)

# Save the plot.

dev.copy(pdf,"boxplot.pdf",width=12,height=6)
dev.off()

# Remove observations will influence our model building.

EmissionsData <- EmissionsData[-c(750,752,753,754,760),] 

table(EmissionsData$FuelType)

par(mfrow = c(1,1))
plot(EmissionsData$Year, EmissionsData$CO2, xlab = "Year", ylab = "CO2", 
     main = "Relationship between year and CO2 emissions")


plot(EmissionsData$MfrGroup, EmissionsData$CO2, xlab = "Manufacturer", ylab = "CO2", 
     main = "Relationship between manufacturer and CO2 emissions")

######################################################################
##                                                                  ##
##            4. Check normality of response variable               ##
##                                                                  ##
######################################################################

# QQplot to check whether response variable are normally distributed.
par(mfrow = c(1,2))

qqnorm(EmissionsData$CO2)
qqline(EmissionsData$CO2, col = "red", lty = 2)

# Exponential QQplot to check whether response variable are appoximately exponential distributed.

p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(EmissionsData$CO2,p=p,na.rm = TRUE) # percentiles of the sample distribution
plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=qexp,col="red", lty=2)
rm(q);rm(p)
dev.copy(pdf,"QQplot.pdf",width=12,height=6)
dev.off()
par(mfrow = c(1,1))


####################################################################
##                                                                ##
##                  PART III: Model Building                      ##
##                                                                ##
####################################################################

####################################################################
##                                                                ##
##        1.  Model building for electric vehicles                ##
##                                                                ##
####################################################################

# Create a dataset only contains 0 CO2 emissions (i.e. elctric vehicles).

electric_car <- EmissionsData[which(EmissionsData$FuelType == "ELECTRIC"),] 

# Use PC1 to predict CO2. 

electric_lm <- lm(CO2 ~ PC1, data = electric_car)
summary(electric_lm)

##################################################################
##                                                              ##
##        2. Build a model for non-electric vehicles.           ##
##                                                              ##
##################################################################

# Create a dataset contains non-electric CO2-missing observations.
ExploratoryData <- EmissionsData[-(EmissionsData$FuelType != "ELECTRIC"),]
non_missing_CO2 <- ExploratoryData[which(ExploratoryData$ID < 14104),]
non_electric_pred <- non_missing_CO2[which(non_missing_CO2$FuelType != "ELECTRIC"),]

# Test whether a linear model fits.

lm1 <- lm(CO2 ~ PC1 + PC2 + PC3 + MfrGroup + Year + FuelType, data = non_missing_CO2, weights = Weights)
summary(lm1)

par(mfrow = c(2,2))
plot(lm1,which = 1:4)
par(mfrow = c(1,1))

# In order to make a gamma GLM, we need do remove observations with 0 in response variables.

non_missing_CO2 <- non_missing_CO2[which(non_missing_CO2$CO2 != 0),]
# First GLM with only main effect. 

glm1 <- glm(CO2 ~ PC1 + PC2 + PC3 + Year + FuelType + MfrGroup ,
            data = non_missing_CO2, family = Gamma(link = "log"), weights = Weights )
par(mfrow = c(2,2))
plot(glm1,which = 1:4)
par(mfrow = c(1,1))
dev.copy(pdf,"residualplot.pdf",width=12,height=12)
dev.off()
summary(glm1)

# Remove the influential point.

non_missing_CO2 <- non_missing_CO2[-which(non_missing_CO2$ID == 11634),]

glm1 <- glm(CO2 ~ PC1 + PC2 + PC3 + Year + FuelType + MfrGroup ,
            data = non_missing_CO2, family = Gamma(link = "log"), weights = Weights )

# Find the R-squared of glm1

R_squared <- 1-(glm1$deviance/glm1$null.deviance)

# MfrGroup might have interaction with PC1 to PC3.

glm2 <- update(glm1, .~.  + MfrGroup:PC1 + MfrGroup:PC2 + MfrGroup:PC3)

summary(glm2)

R_squared2 <- 1-(glm2$deviance/glm2$null.deviance)

par(mfrow = c(2,2))
plot(glm2, which = 1:4)
par(mfrow = c(1,1))

anova(glm1, glm2, test="F")

# Update our GLM model with interactions between Year and PC1 - PC3

glm3 <- update(glm2, .~. + Year:PC1 + Year:PC2 + Year:PC3)
summary(glm3)



#########################################################################
##                                                                     ##
##                Part IV: Prediction                                  ##
##                                                                     ##
#########################################################################


# Use electric_lm to predict the CO2 for electric vehicles  

electric_car_pred <- electric_car[is.na(electric_car$CO2),]
electric_pred <- predict(electric_lm, newdata = data.frame(electric_car_pred),se.fit = TRUE)
electric_pred

# Create a matrix containing ID, CO2 and standard error.  

prediction_electric <- electric_car_pred[,(1:3)]
prediction_electric[,2] <- electric_pred$fit
prediction_electric[,3] <- electric_pred$se.fit

# Use our glm2 to predict our missing CO2.

pred_data1 <- EmissionsData[which(EmissionsData$ID >= 14105),]

pred_data <- pred_data1[which(pred_data1$FuelType != "ELECTRIC"),]

pred <- predict(glm2, newdata = data.frame(pred_data),se.fit = TRUE,type = "response")

# Find the standard deviation of our prediction for non-electric vehicles.

glm_summary <- summary(glm2)

var_hat1 <- (pred$se.fit)^2

var_hat2 <- (glm_summary$dispersion)*(pred$fit^2)

sd <- sqrt(var_hat1 + var_hat2)

# Create the dataframe for our prediction.

prediction <- pred_data[,(1:3)]
prediction[,2] <- pred$fit 
prediction[,3] <- sd

# Combine electric_pred and pred together and sort them in right order.

prediction <- rbind.data.frame(prediction, prediction_electric, make.row.names = FALSE)

prediction <- prediction[order(prediction$ID),]

names(prediction) <- c("", "", "")

# Save the data file.

write.table(prediction,file = "15043372_pred.dat", row.names = FALSE, col.names = FALSE,sep = " ")

