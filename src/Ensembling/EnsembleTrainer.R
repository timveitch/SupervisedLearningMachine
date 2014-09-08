TrainEnsemble <- function(trainers, blender, folds.list.generator, seed) {
  function(df.x,y,w,support) {
    outputs = lapply(trainers, function(trainer) {
      TrainModelWithCV(trainer, folds.list.generator, seed)(df.x,y,w,support)
    })
    num.models = length(trainers)
    
    all.cv.preds = lapply(1:num.models, function(model) {
      outputs[[model]]$cv.preds
    })
    
    df.cv.preds = MakePredictionsDF(all.cv.preds)
    
    blended.output = TrainModelWithCV(blender,folds.list.generator,seed)(df.cv.preds,y,w,support)
    
    predictor <- function(new.x) {
      all.test.preds = lapply(1:num.models, function(model) {
        outputs[[model]]$predict(new.x)
      })
      blended.output$predict(MakePredictionsDF(all.test.preds))
    }
    
    list(predict = predictor, model = outputs, model.blender = blended.output, cv.preds = blended.output$cv.preds)
  }
}

MakePredictionsDF = function(all.preds) {
  df.preds = as.data.frame(all.preds)
  colnames(df.preds) = paste("M",1:ncol(df.preds),sep="")
  df.preds
}