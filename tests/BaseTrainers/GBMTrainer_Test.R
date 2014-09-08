# test glmnet

library(ISLR)
data(Hitters)

subset = Hitters[-which(is.na(Hitters$Salary)),]

y = subset$Salary
x = subset[,-which(colnames(subset) == "Salary")]
w = rep(1, length(y))
support = NULL
seed = 1
parallel = T

gbm.params = DefaultGBMParams()
gbm.params$distribution = "gaussian"
gbm.params$shrinkage = 0.001

# NEED TO PASS THESE PARAMS THROUGH
# tree.interval, max.trees, give.up.after.n.intervals, loss.function, folds.list.generator
tree.interval = 400
max.trees = 15000
give.up.after.n.intervals = 4
loss.function = WeightedMSE
folds.list.generator = SimpleFoldsListGenerator(1,8)

#cv.trainer = TrainGBMWithCV(tree.interval, max.trees, give.up.after.n.intervals, loss.function, folds.list.generator)(gbm.params)
cv.trainer = TrainGBMWithLongStep(8000,4000,1000,folds.list.generator)(gbm.params)

start.time = proc.time()
output = cv.trainer(x,y,w,support,seed,parallel)
end.time = proc.time()
print(paste("Time:",(end.time - start.time)[3]))
loss = loss.function(output$cv.preds, y, w)

AssertMatch(84109.4, loss, WithinDelta(1),  "MSE")
AssertMatch(8849, output$best.trees, Equal, "Best.Trees")
AssertMatch(561.4524, output$predict(x[1,]), WithinDelta(0.0001), "Predict x1")