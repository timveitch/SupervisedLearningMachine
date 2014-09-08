TuneHyperParameters <- function(cv.trainer, response.surface.trainer, params.universe, folds.list.generator, loss.function, seed, params.prob = rep(1,nrow(params.universe))) {
  function(df.x,y,w,support) {
    sample.size   = 50
    
    params.subset = RandomlyChooseParameters(params.universe, params.prob, sample.size)
    #params.subset[1,1] = 5
    #params.subset[1,2] = 8
    #params.subset[1,3] = 0.6
    #params.subset = params.subset[1,,drop = F]
    #test.losses = params.subset[,1] - params.subset[,2]
    test.losses = CalculateLossesForParamsSubset(cv.trainer,params.subset,loss.function)(df.x,y,w,support)
    
    response.output = response.surface.trainer(params.subset, test.losses, rep(1, nrow(params.subset)), NULL)
    
    response.universe.preds = response.output$predict(params.universe)
    
    best = which.min(response.universe.preds)
    predicted.loss = response.universe.preds[best]
    print(paste("Best predicted loss",predicted.loss))
    plot(test.losses, response.output$cv.preds)
    test.results = cbind(params.subset,test.losses)
    print(test.results)

    best.params = MakeIntoList(params.universe[best,,drop=F])
    
    best.output = cv.trainer(best.params)(df.x,y,w,support)
    best.output$best.params = best.params  
    best.output$response.surface = cbind(params.universe, response.universe.preds)
    best.output$test.results = test.results
    best.output
  }
}

CalculateLossesForParamsSubset = function(cv.trainer,params.subset,loss.function) {
  function(df.x,y,w,support) {
    sapply(1:nrow(params.subset), function(row) {
      print(paste("Parameter set:",row))
      print(params.subset[row,])
      tuning.params = MakeIntoList(params.subset[row,,drop=F])
      CalculateLossForParams(cv.trainer,tuning.params,loss.function)(df.x,y,w,support)
    })
  }
}

CalculateLossForParams = function(cv.trainer,tuning.params,loss.function) {
  function(df.x,y,w,support) {
    #print(tuning.params)
    #print("_-------")
    test.output = cv.trainer(tuning.params)(df.x,y,w,support)
    #loss = test.output$biased.cv.loss
    loss = if (!is.null(test.output$cv.preds)) {
      loss.function(test.output$cv.preds, y, w)
    } else if (!is.null(test.output$biased.cv.loss)) {
      test.output$biased.cv.loss
    } else {
      stop("CV Preds / Loss can not be determined")
    }
    loss
  }
}

RandomlyChooseParameters <- function(params.set, weights, sample.size) {
  scaled.weights = weights / sum(weights)
  sample.rows = sample.int(nrow(params.set), size = sample.size, prob = scaled.weights)
  params.set[sample.rows,,drop = F]
}

MakeIntoList = function(params.df.row) {
  params.list = list()
  if (nrow(params.df.row) != 1) {
    stop("expecting a data frame with 1 row")
  }
  for (col in colnames(params.df.row)) {
    params.list[[col]] = params.df.row[1,col]
  }
  params.list
}
