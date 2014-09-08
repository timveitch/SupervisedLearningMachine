
RFTrainer <- function(params) { 
  library(randomForest)
  function(df.x, y, w, df.support) {
    
    strata <- if (!is.null(params$strata.generator)) {
      params$strata.generator(df.x,y,w,df.support)
    } else {
      NULL
    }
    args <- list(x = df.x, y = y)
    if (!is.null(params$ntree)) { args["ntree"] = params$ntree } 
    if (!is.null(params$mtry.fraction)) { 
      args[["mtry"]] = round(params$mtry.fraction * ncol(df.x)) 
    }
    if (!is.null(params$replace)) { args[["replace"]] = params$replace }
    if (!is.null(strata)) { args[["strata"]] = strata }
    if (!is.null(params$sample.fraction)) { 
      args[["sampsize"]] = round(params$sample.fraction * length(y))
    }
    if (!is.null(params$nodesize)) { args[["nodesize"]] = params$nodesize }
    rf <- do.call("randomForest", args)
    #rf <- randomForest(df.x, y, ntree = params$ntree, mtry = params$mtry, 
    #             replace = params$replace, strata = strata, 
    #             sampsize = params$sampsize, nodesize = params$nodesize)
    
    predictor <- function(df.x.test) {
      if (is.factor(y)) {
        as.vector(predict(rf, df.x.test,type = "prob")[,2])
      } else {
        predict(rf, df.x.test)
      }
    }
    return(list(predict = predictor,model = rf))
  }
}

MakeRFParams = function(ntree,mtry.fraction,sample.fraction,nodesize,strata) {
  list(ntree = ntree, mtry.fraction = mtry.fraction, sample.fraction = sample.fraction, nodesize = nodesize, replace = F, strata = strata)
}

MakeRFParamsList = function(ntree,mtry.fractions,sample.fractions,nodesizes,strata) {
  count = 0
  list  = list()
  
  for (mtry.fraction in mtry.fractions) {
    for (sample.fraction in sample.fractions) {
      for (nodesize in nodesizes) {
        count = count + 1
        list[[count]] = MakeRFParams(ntree, mtry.fraction, sample.fraction, nodesize, strata)
      }
    }
  }
  return(list)
}