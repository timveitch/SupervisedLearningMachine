
TrainCVEnsemble <- function(fraction.cv = 0.5) {
  function(trainer, folds.list.generator, seed) {
    function(df.x, y, w, support) {
      m <- TrainModelWithCV(trainer,folds.list.generator,seed)(df.x,y,w,support)
      MakeIntoCVEnsemble(m)
    }
  }
}

MakeIntoCVEnsemble <- function(model.output.with.cv, fraction.cv = 0.5) {
  cv.predictor <- function(new.x) {
    # average the CV model predictions
    summed.cv.predictions = rep(0,nrow(new.x))
    count = 0
    for (fold.outputs in model.output.with.cv$cv.outputs) {
      for (cv.output in fold.outputs) {
        count = count + 1
        summed.cv.predictions = summed.cv.predictions + cv.output$predict(new.x)  
      }
    }
    summed.cv.predictions / count
  }
  predictor <- function(new.x) {
    cv.predictor(new.x) * fraction.cv + model.output.with.cv$predict(new.x) * (1 - fraction.cv)
  }
  new.output         = model.output.with.cv
  new.output$predict = predictor
  new.output
}