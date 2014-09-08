# General process for simultaneously performing cross validation
# and training a model on the full data set

TrainModelWithCV <- function(trainer, folds.list.generator = SimpleFoldsListGenerator(1,8)) {
  function(df.x, y, w, df.support, seed, parallel) {
    folds.list <- folds.list.generator(df.x,y,w,df.support,seed)
    r = 0
    cv.outputs <- list()
    
    if (parallel) {
      library(doParallel)
    }
    
    for (folds in folds.list) {
      r = r + 1
      fold.levels <- sort(unique(folds))
      fold.function = function(fold) {
        fold.seed = FoldSeed(seed,r,fold)
        fold.parallel = F
        
        print(paste("Fold:", fold))
        df.x.train <- df.x[folds != fold,,drop=F]
        y.train    <- y[folds != fold]
        w.train    <- w[folds != fold]
        
        df.support.train <- df.support[folds != fold,,drop=F]
        
        output = trainer(df.x.train,y.train,w.train,df.support.train,fold.seed,fold.parallel)
        output$fold = fold
        output
      }
      applier = if (parallel) { function(list,fun) {
        mclapply(list,fun,mc.cores = getDoParWorkers()) }  
      } else { lapply }
      cv.outputs[[r]] = applier(fold.levels, fold.function)
      gc()
    }
    
    final.output     <- trainer(df.x,y,w,df.support,seed,parallel)
    MakeModelOutput(final.output, cv.outputs, df.x, folds.list)
  }
}

MakeModelOutput = function(final.output, cv.outputs, df.x, folds.list) {
  preds.all <- rep(0, length(folds.list[[1]]))
  r = 0
  for (folds in folds.list) {
    r = r + 1
    fold.levels <- sort(unique(folds))
    for (fold in fold.levels) {
      cv.output <- cv.outputs[[r]][[fold]]
      df.x.validate = df.x[folds == fold,,drop=F]
      preds.fold = cv.output$predict(df.x.validate)
      preds.all[folds == fold] = preds.all[folds == fold] + preds.fold
    }
  }
  preds.all         = preds.all / r
  output            = final.output
  output$cv.preds   = preds.all
  output$cv.outputs = cv.outputs
  output$folds.list = folds.list
  
  return(output)
}

ApplyToCVOutputs <- function(model.output, fun) {
  result = model.output
  if (!is.null(model.output$cv.outputs)) {
    result$cv.outputs = lapply(model.output$cv.outputs, function(fold.outputs) {
      lapply(fold.outputs, fun)
    })
  }
  result
}

ApplyToFinalAndCVOutputs <- function(model.output, fun) {
  result = fun(model.output)   # apply to top level outputs
  ApplyToCVOutputs(result,fun) # apply to cv outputs
}
