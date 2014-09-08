# test glmnet

library(ISLR)
data(Hitters)

subset = Hitters[-which(is.na(Hitters$Salary)),]

y = subset$Salary
x = subset[,-which(colnames(subset) == "Salary")]
w = rep(1, length(y))
support = NULL
seed = 1
parallel = F

grid = 10^seq(10,-2,length=100)

glmnet.params = list(family = "gaussian", alpha = 0.0, lambda = grid,nfolds = 10)

output = TrainCVGlmnet(glmnet.params)(x,y,w,support,seed,parallel)

delta1 = WithinDelta(0.001)
AssertMatch(231.013, output$lambda.min,           delta1, "LambdaMin")
AssertMatch(10.089,  output$coefficients[1],      delta1, "Intercept")
AssertMatch(0.0433,  output$coefficients[2],      delta1, "AtBat")
AssertMatch(20,      length(output$coefficients), Equal,  "Num Coefficients")

glmnet.params$alpha = 1
output = TrainCVGlmnet(glmnet.params)(x,y,w,support,seed,parallel)
AssertMatch(0,      output$coefficients[5], Equal,  "Runs")