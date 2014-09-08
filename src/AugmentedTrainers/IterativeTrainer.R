
StepFinalAndCVOutputs = function(model.output, stepper, iteration, x, y, w, support, parallel) {
  new.seed         = model.output$seed + iteration * 10
  new.final.output = stepper(model.output, iteration, x, y, w, support, new.seed, parallel)
  r = 0
  new.cv.outputs = list()
  for (folds in model.output$folds.list) {
    r = r + 1
    new.cv.outputs[[r]] = list()
    
    fold.func <- function(fold.output) {
      fold.parallel = FALSE
      train = which(folds != fold.output$fold)
      x.subset <- x[train,,drop=F]
      y.subset <- y[train]
      w.subset <- w[train]
      support.subset <- if (is.null(support)) {
        NULL
      } else {
        support[train,,drop=F]
      }
      fold.seed = fold.output$seed + iteration * 10
      stepper(fold.output, iteration, x.subset, y.subset, w.subset, support.subset, fold.seed, fold.parallel)
    }
    applier = if (parallel) { mclapply } else { lapply } 
    new.cv.outputs[[r]] = applier(model.output$cv.outputs[[r]],fold.func)
  }
  
  MakeModelOutput(new.final.output, new.cv.outputs, x, model.output$folds.list)
}

TrainIterativeModel <- function(trainer,stepper,folds.list.generator,max.iterations,give.up.after.n.failures, loss.function) {
  function(x,y,w,support,seed,parallel) {
    model.output <- TrainModelWithCV(trainer, folds.list.generator)(x,y,w,support,seed,parallel) 
    best.loss    <- loss.function(model.output$cv.preds, y, w)
    best.iterations <- 1
    best.output     <- model.output
    
    print(paste("Iter: 1, Loss:",round(best.loss,5)))
    iteration.losses <- best.loss
    
    give.up.count = 0
    
    for (iteration in 2:max.iterations) {
      print(give.up.count)
      print(give.up.after.n.failures)
      if (give.up.count == give.up.after.n.failures) {
        break
      }
      model.output = StepFinalAndCVOutputs(model.output,stepper,iteration,x,y,w,support,parallel)
      
      loss <- loss.function(model.output$cv.preds, y, w)
      iteration.losses <- c(iteration.losses,loss)
      print(paste("Iter: ",iteration,", Loss: ", round(loss,5), sep = ""))
      if (loss < best.loss) {
        best.loss  = loss
        best.iterations = iteration
        best.output     = model.output
        give.up.count = 0
      } else {
        give.up.count = give.up.count + 1
      }
    }
    gc()
    print(paste("best loss:",best.loss))
    best.output$biased.cv.loss    = best.loss
    best.output$best.iterations   = best.iterations
    best.output$iteration.losses  = iteration.losses
    best.output
  }
}