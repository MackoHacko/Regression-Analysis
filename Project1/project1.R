library(ISLR)
library(pls)
library(glmnet)
library("hdi")
data(riboflavin)

# Load dataset, randomize and create train / test
set.seed (1000)
n <- nrow(riboflavin)
train <- sample(1:n,0.75*n) # 2/3 for training
test <- (-train)
Ribo.test <- riboflavin[,2][test, ]
Ribo.train <- riboflavin[,2][train, ]
x.train <- data.matrix(Ribo.train)
x.test <- data.matrix(Ribo.test)
y.train <- riboflavin[,1][train]
y.test <- riboflavin[,1][test]

# Principal component rergession (PCR)
pcr.fit <- pcr(y.train ~ x.train, scale = TRUE, validation = "CV", segment.type = "consecutive", segments = 3)
#summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSE",legendpos = "topright") # MSEP, R2, RMSEP
pcr.pred <- predict(pcr.fit, x.test, ncomp = 18)
mse.pcr <- mean((pcr.pred - y.test)^2)
mse.pcr

# Partial least squares regression (PLS)
pls.fit = plsr(y.train ~ x.train, scale = TRUE, validation = "CV", segment.type = "consecutive", segments = 3)
#summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP",legendpos = "topright")
pls.pred = predict(pls.fit, x.test, ncomp = 4)
mse.pls <- mean((pls.pred - y.test)^2)

# Ridge regression
cv.ridge <- cv.glmnet(x.train, y.train, alpha = 0)
cv.ridge$lambda.min
ridge.pred <- predict(cv.ridge, s = "lambda.min", newx = x.test)
mse.cv.ridge <- mean((ridge.pred - y.test)^2)


# Lasso regression
cv.lasso <- cv.glmnet(x.train, y.train, alpha = 1)
cv.lasso$lambda.min
lasso.pred <- predict(cv.lasso, s = "lambda.min", newx = x.test)
mse.cv.lasso <- mean((lasso.pred - y.test)^2)

# Splitting Lasso, hdi() kör lasso per default.
# Den enda genen som är signifikant enligt denna metod är 'YXLD_at'
set.seed(1337)
fit.multi <- hdi(riboflavin[,-1], riboflavin[,1], B=100, ci.level = 0.90)

fit.multi$pval.corr['YXLD_at'] # Printar genens p-värde
fit.multi$lci['YXLD_at'] # Printar upper confidence bound
fit.multi$uci['YXLD_at'] # Printar lower confidence bound
