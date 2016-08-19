creditDefault <- read.csv(file.choose())

colnames(creditDefault)


str(creditDefault)
summary(creditDefault)

creditDefault$default_paymentNextMonth <- as.factor(creditDefault$default_paymentNextMonth)
creditDefault$SEX <- as.factor(creditDefault$SEX)
creditDefault$EDUCATION <- as.factor(creditDefault$EDUCATION)
creditDefault$MARRIAGE <- as.factor(creditDefault$MARRIAGE)
creditDefault$PAY_0 <- as.factor(creditDefault$PAY_0)
creditDefault$PAY_2 <- as.factor(creditDefault$PAY_2)
creditDefault$PAY_3 <- as.factor(creditDefault$PAY_3)
creditDefault$PAY_4 <- as.factor(creditDefault$PAY_4)
creditDefault$PAY_5 <- as.factor(creditDefault$PAY_5)
creditDefault$PAY_6 <- as.factor(creditDefault$PAY_6)


#Set the seed of R's random number generator, which is useful for creating simulations 
#or random objects that can be reproduced.
#seed number we choose is the starting point used in the generation 
set.seed(9000)
test <- sample(nrow(creditDefault),0.3*nrow(creditDefault))  
data.train <- creditDefault[-test,]  
data.test <- creditDefault[test,]   

str(data.train)

nrow(data.train)
nrow(data.test)

library(leaps)
model1 <- regsubsets(default_paymentNextMonth ~ LIMIT_BAL+SEX+EDUCATION+
                       MARRIAGE+AGE+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+BILL_AMT1+
                       BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6+PAY_AMT1+
                       PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6, data=data.train, nvmax=23)
summary(model1)
plot(model1, scale="adjr2")
plot(model1, scale="bic")
