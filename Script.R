#------------------------------------------------------
# Importing data

dset <- read.csv("insurance.csv", header = TRUE); 
head(dset)

#------------------------------------------------------
# Part 1 

# Frequency distribution of each of the categorial variables

table(dset$sex)
table(dset$smoker)
table(dset$region)

# Summary to get basic statistics
summary(dset)

# Histogram to check normal distribution
hist(dset$bmi)
hist(dset$expenses)

# Log of expences as a new variable 
dset$Lexpenses <- log(dset$expenses); View(dset)
hist(dset$Lexpenses)

#------------------------------------------------------
# Part 2

# scatter plot matric of numeric variables
pairs(dset[c("age","bmi","children","expenses")])

# Plot of categorical variables with expences 
plot(dset$sex,dset$expenses)
plot(dset$smoker,dset$expenses)
plot(dset$region,dset$expenses)


# Repeat for log expences 
pairs(dset[c("age","bmi","children","Lexpenses")])

plot(dset$sex,dset$Lexpenses)
plot(dset$smoker,dset$Lexpenses)
plot(dset$region,dset$Lexpenses)

#------------------------------------------------------
# Part 3

#Multiple regression

model1 <- lm(dset$expenses~dset$age+dset$sex+dset$bmi+dset$children+dset$smoker+dset$region);summary(model1)             
model2 <- lm(dset$Lexpenses~dset$age+dset$sex+dset$bmi+dset$children+dset$smoker+dset$region);summary(model2)   
anova(model1)
anova(model2)

fits1 <- fitted(model1)
resids1 <- residuals(model1)
plot(fits1, resids1, main = "Residual-Fits Plot model 1", xlab = "Fits", ylab = "Residuals")

fits2 <- fitted(model2)
resids2 <- residuals(model2)
plot(fits2, resids2, main = "Residual-Fits Plot model 2", xlab = "Fits", ylab = "Residuals")

#------------------------------------------------------
# Part 4

dset$SQage <- dset$age^2; View(dset)

model3 <- lm(dset$expenses~dset$age+dset$SQage+dset$sex+dset$bmi+dset$children+dset$smoker+dset$region);summary(model3)  
summary(model3)
model4 <- lm(dset$expenses~dset$age+dset$SQage+dset$sex+dset$bmi+dset$children+dset$smoker+dset$region+dset$bmi*dset$smoker);summary(model4) 
summary(model4)

fits3 <- fitted(model3)
resids3 <- residuals(model3)
plot(fits3, resids3, main = "Residual-Fits Plot model 3", xlab = "Fits", ylab = "Residuals")


fits4 <- fitted(model4)
resids4 <- residuals(model4)
plot(fits4, resids4, main = "Residual-Fits Plot model 4", xlab = "Fits", ylab = "Residuals")



