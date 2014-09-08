ParameterTrainer <- function(test.trainer, params.list, loss.function, folds.list.generator, seed, final.trainer = NULL ) {
  function(df.x, y, w, df.support) {
    set.seed(seed)
    best = NULL
    best.loss = NULL
    best.output = NULL
    losses <- c()
    count = 0
    for (params in params.list) {
      print(paste("Parameter set",count))
      count = count + 1
      m = TrainModelWithCV(test.trainer(params),folds.list.generator,seed)(df.x, y, w, df.support)
      loss = loss.function(m$cv.preds,y,w)
      losses <- c(losses, loss)
      print(paste("CV LOSS:", loss))
      if (is.null(best.loss)) { best.loss = loss + 1 }
      if (loss < best.loss) {
        best.loss = loss
        best.output = m
        best = count
      }
    }
    
    best.params <- params.list[[best]]
    
    result = if (!is.null(final.trainer)) {
      final.trainer(best.params)(df.x, y, w,df.support)
    } else {
      best.output
    }
    result$best.params = best.params
    result$individual.losses = losses
    return(result)
  }
}
