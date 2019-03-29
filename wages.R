library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(reshape2)
library(stringr)
library(TSA)
library(stats)
library(lmtest) # durbin-watson test
library(nortest) #normality test
library(MASS) #standardized Residuals
library(randomForest)
Rawdata = read.csv('C:/Users/admin/Downloads/CompleteDataset.csv')
colnames(Rawdata)
Rawdata <- Rawdata[,c(2, 3, 4, 5, 7, 8, 9, 11, 12)]
Rawdata$Value_new <- as.numeric(str_extract(Rawdata$Value, "[0-9]+"))
Rawdata$Value_unit <- str_extract(Rawdata$Value, "[aA-zZ]+")
Value_K_index <- which(Rawdata$Value_unit %in% "K")
Rawdata$Value_new[Value_K_index] <- Rawdata$Value_new[Value_K_index] * 1000
Value_M_index <- which(Rawdata$Value_unit %in% "M")
Rawdata$Value_new[Value_M_index] <- Rawdata$Value_new[Value_M_index] * 1000000 
Rawdata$Wage_new <- as.numeric(str_extract(Rawdata$Wage, "[0-9]+")) 
Rawdata$wage_unit <- str_extract(Rawdata$Wage, "[aA-zZ]+")
Wage_K_index <- which(Rawdata$wage_unit %in% "K")
Rawdata$Wage_new[Wage_K_index] <- Rawdata$Wage_new[Wage_K_index] * 1000
#exclude someone who don't earn any money
Rawdata <- Rawdata[Rawdata$ Wage_new !=0, ]
#What level is a club in? high, medium, low
r2 <- by(Rawdata$Value_new, Rawdata$Club, mean)
help(by)
r2 <- r2[-1]
club_average <- data.frame(name = rownames(r2), value_average = as.numeric(r2))
low_club <- club_average[club_average$value_average<= quantile(club_average$value_average, c(0.33)),]
medium_club <-  club_average[(club_average$value_average> quantile(club_average$value_average, c(0.33))) &(club_average$value_average<= quantile(club_average$value_average, c(0.66))),]
high_club <- club_average[club_average$value_average > quantile(club_average$value_average, c(0.66)),]
Rawdata$clublevel[Rawdata$Club %in% low_club$name] <- 1
Rawdata$clublevel[Rawdata$Club %in% medium_club$name] <- 2
Rawdata$clublevel[Rawdata$Club %in% high_club$name] <- 3
Rawdata$clublevel <- as.factor(Rawdata$clublevel)
#Numeric Rawdata
Numeric_Rawdata <- Rawdata[c(2,5,6,10,12,14)]
#Scatter between numeric values
pairs(Numeric_Rawdata[,1:5], panel = panel.smooth)
#From the plot, especially for the 5 th row, you can see the relationship between each independent variables and dependent variable(wage_new). Except for value_new, there is nothing tha has a linear relationship with wage_new. However, as for overall and potential, it seems to have a exponential relationship with wage variable. so I'll take the natural log of these two variables to have a linear relationship.
#Logarithm the overall and potential
log_numeric_Rawdata <- transform(Numeric_Rawdata, Overall_ln = log(Overall), Potential_ln = log(Potential), wage_ln = log(Wage_new), clublevel = clublevel)
log_numeric_Rawdata1 <- log_numeric_Rawdata[,c(1, 7, 8, 9)]
#Scatter between numeric values
pairs(log_numeric_Rawdata1, panel = panel.smooth)
#Independence test for dependent variable
par(mfrow = c(1,2))
lag.plot(Numeric_Rawdata$Wage_new, set = c(1:6), main = "Wage - Time Lag Plot")
lag.plot(log_numeric_Rawdata$wage_ln, set = c(1:6), main = "log Wage - Time Lag plot")
#Fit multiple linear regresion to the log Rawdata
lm1 <- lm(log_numeric_Rawdata$wage_ln ~ log_numeric_Rawdata$Overall_ln + log_numeric_Rawdata$Potential_ln + log_numeric_Rawdata$clublevel)
summary(lm1)
dwtest(lm1, alternative = "less") #durbin watson
#Residual QQ plot 
plot(lm1, which =2)
ad.test(stdres(lm1))
#equal variance
bptest(lm1)
