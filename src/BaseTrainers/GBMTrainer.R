
TrainGBM <- function(gbm.params) { 
  library(gbm)
  function(x, y, w, support, seed, parallel) {
    set.seed(seed)
    #if (parallel) {
      #warning("No parallel option available for TrainGBM")
    #}
    
    args = ExpandGBMParams(gbm.params,x,y,w)
    gbm <- do.call("gbm.fit", args)
    
    return(MakeGBMOutput(gbm, seed))
  }
}

ExpandGBMParams <- function(params,x,y,w) {
  args <- list(x = x, y = y, w = w)
  if (!is.null(params$distribution))      { args[["distribution"]] = params$distribution }
  if (!is.null(params$n.trees))           { args[["n.trees"]] = params$n.trees }
  if (!is.null(params$interaction.depth)) { args[["interaction.depth"]] = params$interaction.depth }
  if (!is.null(params$n.minobsinnode))    { args[["n.minobsinnode"]] = params$n.minobsinnode }
  if (!is.null(params$shrinkage))         { args[["shrinkage"]] = params$shrinkage }
  if (!is.null(params$bag.fraction))      { args[["bag.fraction"]] = params$bag.fraction }
  if (!is.null(params$keep.data))         { args[["keep.data"]] = params$keep.data }
  if (!is.null(params$nTrain))            { args[["nTrain"]] = params$nTrain }
  
  args$verbose = F
  args
}

GBMStepper <- function(params, n.new.trees) {
  function(output, iteration,x,y,w,support,seed,parallel) {
    set.seed(seed)
    new.gbm <- gbm.more(output,n.new.trees = n.new.trees)
    MakeGBMOutput(new.gbm, seed)
  }
}

GBMIterativeCVTrainer <- function(trainer, stepper) {
  function(tree.interval, max.trees, give.up.after.n.intervals, loss.function, folds.list.generator) {
    function(params) {
      params[["n.trees"]]   = tree.interval
      max.iterations = ceiling(max.trees / tree.interval)
      TrainIterativeModel(trainer(params), stepper(params,tree.interval), folds.list.generator, max.iterations, give.up.after.n.intervals, loss.function)
    }
  }
}

GBMMoreTrainer <- function(params) {
  params[["keep.data"]] = T
  TrainGBM(params)
}

TrainIterativeGBM <- GBMIterativeCVTrainer(GBMMoreTrainer,GBMStepper)

MakeGBMOutput = function(gbm, seed) {
  output         = gbm
  output$predict = GBMPredictor(gbm)
  output$seed    = seed
  output
}

GBMPredictor <- function(gbm) { 
  function(x) {
    predict(gbm,newdata = x, n.trees = GBMNumTrees(gbm))
  }
}

GBMNumTrees = function(gbm) {
  length(gbm$train.error)
}

MakeGBMParamsList = function(n.trees, distribution, interaction.depths, n.minobsinnodes, shrinkage, bag.fractions, keep.data = F) {
  count = 0
  list  = list()
  
  for (interaction.depth in interaction.depths) {
    for (n.minobsinnode in n.minobsinnodes) {
      for (bag.fraction in bag.fractions) {
        count = count + 1
        list[[count]] = list(n.trees = n.trees, distribution = distribution, interaction.depth = interaction.depth, n.minobsinnode = n.minobsinnode, shrinkage = shrinkage, bag.fraction = bag.fraction, keep.data = keep.data)
      }
    }
  }
  return(list)
}

DefaultGBMParams <- function() {
  r = list()
  r$ntrees            = 100
  r$interaction.depth = 2
  r$shrinkage         = 0.01
  r
}