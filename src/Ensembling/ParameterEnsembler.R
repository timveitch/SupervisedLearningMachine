ParameterEnsembler <- function(trainer, ensemble.trainer, params.list, loss.function, folds.list.generator, seed) {
  function(df.x, y, w, df.support) {
    set.seed(seed)
    model.outputs <- lapply(params.list, function(params) {
      TrainModelWithCV(trainer(params), folds.list.generator, seed)(df.x, y, w, df.support)
    })
    individual.losses <- sapply(model.outputs, function(model.output) {
      loss.function(model.output$cv.preds, y, w)
    })

    best.individual = which.min(individual.losses)
    best.individual.loss = individual.losses[best.individual]
    best.individual.output = model.outputs[[best.individual]]
        
    df.x.ensemble = as.data.frame(sapply(model.outputs, function(model.output) {
      model.output$cv.preds
    }))
    colnames(df.x.ensemble) = paste("V",1:ncol(df.x.ensemble),sep="")
    
    # should use same seed as in above model builds!
    ensemble = TrainModelWithCV(ensemble.trainer, folds.list.generator, seed)(df.x.ensemble, y, w, df.support)
    ensemble.loss <- loss.function(ensemble$cv.preds, y, w)

    print(paste("Ensemble: ", ensemble.loss, ", BestIndividual: ",best.individual.loss, sep = ""))
    output <- if (ensemble.loss < best.individual.loss) {
      print("Ensemble of parameters improves model")
      
      result = AddEnsemblingLayer(ensemble,model.outputs)  
      result$cv.loss   = ensemble.loss
      result
    } else {
      print("Ensemble of parameters does NOT improve model")
      best.individual.output$cv.loss = best.individual.loss
      best.individual.output
    }
    output$individual.losses <- individual.losses
    output$params.list <- params.list
    output
  }
}

AddEnsemblingLayer <- function(ensemble.output, model.outputs) {
  # this is tricky!
  result = ensemble.output
  predictor <- function(newx) {
    new.x.ensemble <- as.data.frame(sapply(model.outputs, function(model.output) {
      model.output$predict(newx)
    }))
    colnames(new.x.ensemble) = paste("V",1:ncol(new.x.ensemble),sep="")
    ensemble.output$predict(new.x.ensemble)
  }
  result$predict = predictor
  
  if (is.null(ensemble.output$cv.outputs)) {
    return(result)
  }
  
  # need to also add an ensembler around each of the cv.output models
  
  r = 0
  for (fold.outputs in ensemble.output$cv.outputs) {
    r = r + 1
    f = 0
    for (fold.output in fold.outputs) {
      f = f + 1
      fold.predictor <- function(newx) {
        new.x.ensemble <- as.data.frame(sapply(model.outputs, function(model.output) {
          model.output$cv.outputs[[r]][[f]]$predict(newx)
        }))
        colnames(new.x.ensemble) = paste("V",1:ncol(new.x.ensemble),sep="")
        fold.output$predict(new.x.ensemble)
      }
      result$cv.outputs[[r]][[f]]$predict = fold.predictor
    }
  }
  result
  
}