
DataSelector <- function(test.trainer, data.generator.list, loss.function, folds.list.generator, seed, final.trainer = NULL) {
  function(df.x, y, w, df.support) {
    set.seed(seed)
    best = NULL
    best.loss = Inf
    best.output = NULL
    losses <- c()
    count = 0
    
    for (data.generator in data.generator.list) {
      
      count = count + 1

      x.generated <- data.generator(df.x)
      print(colnames(x.generated))
      rm(x.generated)
      
      cv.trainer = TrainModelWithCV(test.trainer,folds.list.generator,seed)
      m = DataGeneratedTrainer(data.generator)(cv.trainer)(df.x, y, w, df.support)
      
      loss = loss.function(m$cv.preds,y,w)
      
      print(paste("loss = ", loss,sep = ""))
      
      losses <- c(losses, loss)
      
      if (loss < best.loss) {
        best.loss = loss
        best.output = m
        best = count
      } else {
        rm(m)
      }
      gc()
    }

    result = if (!is.null(final.trainer)) {
      data.generator = data.generator.list[[best]]
      trainer = DataGeneratedTrainer(data.generator)(final.trainer)
      m = trainer(df.x, y, w,df.support)
    } else {
      best.output
    }
    result$cv.loss = best.loss
    result$individual.losses = losses
    return(result)
  }
}
