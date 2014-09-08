TrainGBMWithLongStep <- function(first.step,subsequent.step,min.clearance,folds.list.generator) {
  function(gbm.params) {
    function(x,y,w,support,seed,parallel) {
      folds.list = folds.list.generator(x,y,w,support,seed)
      r = 0
      cv.outputs = list()
      
      applier = if (parallel) { function(list,fun) {
        mclapply(list,fun,mc.cores = getDoParWorkers()) } 
      } else { lapply }

      for (folds in folds.list) {
        r = r + 1
        fold.func = function(fold) {
          
          i = order(folds == fold)
          ntrain = sum(folds != fold)
          
          new.params           = gbm.params
          new.params$nTrain    = ntrain
          new.params$n.trees   = first.step
          new.params$keep.data = T
          
          x.ord = x[i,,drop = F]
          y.ord = y[i]
          w.ord = w[i]
          support.ord = if (is.null(support)) {
            NULL
          } else {
            support[i,,drop = F]
          }
          
          TrainGBM(new.params)(x.ord,y.ord,w.ord,support.ord,seed,F)
        }
        cv.outputs[[r]] = applier(sort(unique(folds)), fold.func)
        
      }
      num.trees = first.step
      best.tree = GetBestTree(cv.outputs, folds.list, num.trees)
      iteration = 1
      while (num.trees - best.tree < min.clearance) {
        iteration = iteration + 1
        # add more trees and recalculate num.trees and best.tree
        r = 0
        for (folds in folds.list) {
          r = r + 1
          fold.func = function(fold) {
            set.seed(seed + iteration*10000)
            gbm = gbm.more(cv.outputs[[r]][[fold]],n.new.trees = subsequent.step)
            MakeGBMOutput(gbm,seed)
          }
          cv.outputs[[r]] = applier(sort(unique(folds)),fold.func)
        }
        num.trees = num.trees + subsequent.step
        best.tree = GetBestTree(cv.outputs, folds.list, num.trees)
      }
      
      # We now know the best number of trees
      # Build the main gbm
      final.params           = gbm.params
      final.params$n.trees   = best.tree
      
      set.predict = function(output) {
        output$predict = function(newx) {
          predict(output, newx, n.trees = best.tree)
        }
        output
      }
      final.output = TrainGBM(final.params)(x,y,w,support,seed,F)
      final.output$predict = set.predict(final.output)
      cv.outputs = lapply(cv.outputs, function(fold.outputs) {
        lapply(fold.outputs, set.predict)
      })
      final.output = MakeModelOutput(final.output, cv.outputs, x, folds.list)
      final.output$best.trees = best.tree
      final.output$cv.error   = CalculateGBMCVError(cv.outputs, folds.list, num.trees)
      ApplyToFinalAndCVOutputs(final.output, set.predict)
    }
  }
}

GetBestTree <- function(cv.outputs, folds.list, num.trees) {
  cv.error.per.tree = CalculateGBMCVError(cv.outputs, folds.list, num.trees)
  which.min(cv.error.per.tree)
}

CalculateGBMCVError <- function(cv.outputs,folds.list,num.trees) {
  total.error = double(num.trees)
  r = 0
  for (folds in folds.list) {
    r = r + 1
    for (fold in sort(unique(folds))) {
      weight = sum(folds == fold)
      total.error = total.error + weight * cv.outputs[[r]][[fold]]$valid.error
    }
  }
  total.error
}