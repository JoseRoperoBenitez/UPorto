data <- read.csv("C:/Users/jrope/Downloads/freMTPL2freq.csv/freMTPL2freq.csv",
                 stringsAsFactors=TRUE)
set.seed(123)
# IDpol: the policy ID
# ClaimNb: number of claims during the exposure period
# Exposure: period of exposure for a policy, in years
# VehPower: the power of the car
# VehAge: The vehicle age, in years
# DrivAge: The driver's age, in years
# BonusMalus: bonus/malus rating of policyholder (between 50 and 350) 
#             <100 means bonus
# VehBrand: brand of the car
# VehGas: car's fuel type
# Area: density-rating of the community that the car driver lives in, divided 
#       into categories: from "A"-rural area to "F"-urban

attach(data)

str(data)
# 678013 observations of 12 variables

summary(data)

# We are going to analice:
# ClaimNb:
summary(ClaimNb)
boxplot(ClaimNb)
hist(ClaimNb)

# DrivAge:
summary(DrivAge)
boxplot(DrivAge)
hist(DrivAge)

# BonusMalus:
summary(BonusMalus)
boxplot(BonusMalus)
hist(BonusMalus)

# Correlation
data_numeric= data[, sapply(data, is.numeric)]
str(data_numeric)

round(cor(data_numeric), 2)
heatmap(abs(cor(data_numeric)), scale = "none",cexRow = 1,cexCol = 0.6)

# The highest correlation is between BonusMalus and DrivAge and has a value of
# -0.48 which is telling as that as the Age increase, the BonusMalus decrese and 
# the opposite. This value is not telling us that much but it's a signal of that 
# insurance companies should take into account the age of the insured.

# Analisis:
rall = lm(BonusMalus~.,data=data_numeric)
summary(rall)

# method step to step
rback=step(rall,direction="backward")
rback

plot(BonusMalus ~ DrivAge)

# CTE
nivel_confianza = 0.95  
columna = data_numeric$ClaimNb
percentil = quantile(columna, probs = nivel_confianza, na.rm = TRUE)
cte = mean(columna[columna > percentil], na.rm = TRUE)
cte #2.085
# This means that for the worst 5% of the cases the expected value of the number 
# of claims is 2.085 or more

# Vs VaR and TVaR
nivel_confianza = 0.95
VaR = quantile(data_numeric$ClaimNb, probs = 1 - nivel_confianza, na.rm = TRUE)
VaR
cola = columna[columna > VaR]
TVaR = mean(cola, na.rm = TRUE)
TVaR #1.059953
# This means that, on average, given that an extreme claim occurs (beyond the 
# VaR), the expected number of claims is approximately 1.06.


#CTE
nivel_confianza = 0.95  
columna2 = data_numeric$DrivAge
percentil = quantile(columna2, probs = nivel_confianza, na.rm = TRUE)
cte = mean(columna2[columna2 > percentil], na.rm = TRUE)
cte
# This means that for the worst 5% of the cases the expected value of the driver
# age is 77.64 or more

# Vs VaR and TVaR
nivel_confianza = 0.95
VaR = quantile(data_numeric$DrivAge, probs = 1 - nivel_confianza, na.rm = TRUE)
VaR
cola = columna2[columna2 > VaR]
TVaR = mean(cola, na.rm = TRUE)
TVaR
# This means that on average, given that a driver’s age falls into the extreme 
# upper tail of the distribution (beyond the VaR), the expected age is
# approximately 46.88 years.


#CTE
nivel_confianza = 0.95  
columna3 = data_numeric$BonusMalus
percentil = quantile(columna3, probs = nivel_confianza, na.rm = TRUE)
cte = mean(columna3[columna3 > percentil], na.rm = TRUE)
cte
# This means that for the worst 5% of the cases the expected value of the 
# BonusMalus is 104.94 or more

# Vs VaR and TVar
nivel_confianza = 0.95
VaR = quantile(data_numeric$BonusMalus, probs = 1 - nivel_confianza,
               na.rm = TRUE)
VaR
cola = columna3[columna3 > VaR]
TVaR = mean(cola, na.rm = TRUE)
TVaR
# This means that on average, given that a policyholder's Bonus-Malus rating is
# in the extreme upper tail of the distribution (beyond the VaR), the expected 
# Bonus-Malus rating is approximately 72.52.

