
RankBaseModels <- function(loss.function,best.to.worst) {
  function(df.x,y,w,support) {
    colnames = colnames(df.x)
    losses   = sapply(colnames, function(colname) {
      loss.function(df.x[,colname],y,w)
    })
    df.losses <- data.frame(model = colnames, loss = losses)
    order = if (best.to.worst) {
      order(losses)
    } else {
      order(-losses)
    }
    df.losses[order,] # rank in descending order of loss (i.e. worst to best)
  }
}

IncrementalBlendingTrainer <- function(loss.function,best.to.worst = T) {
  function(df.x,y,w,support) {
    ranked.models = RankBaseModels(loss.function,best.to.worst)(df.x,y,w,support)
    num.models    = nrow(ranked.models)
    model.fractions = cbind(ranked.models, fraction = rep(0,num.models))
    model.names     = as.vector(model.fractions[,"model"])
    
    # set first model fraction to 1 - starting point
    model.fractions[1,"fraction"] = 1
    first.model = model.names[1]
    blended.preds = df.x[,first.model]
    initial.loss = loss.function(blended.preds,y,w)
    for (i in 2:num.models) {
      new.preds = df.x[,model.names[i]]
      fraction.new = DetermineOptimalFraction(blended.preds,new.preds,y,w,loss.function)
      model.fractions[i,"fraction"] = fraction.new
      fraction.old = (1 - fraction.new)
      for (previous in (i-1):1) {
        model.fractions[previous,"fraction"] = fraction.old * model.fractions[previous,"fraction"]
      }
      blended.preds = fraction.old * blended.preds + fraction.new * new.preds
    }
    
    predictor = function(newx) {
      result = rep(0,nrow(newx))
      for (i in 1:num.models) {
        fraction = model.fractions[i,"fraction"]
        model    = model.names[i]
        result = result + fraction * newx[,model]
      }
      result
    }
    print(paste("final loss:",loss.function(blended.preds,y,w)))
    print(model.fractions)

    return(list(predict = predictor, model = model.fractions))
  }
}

DetermineOptimalFraction <- function(blended.preds,new.preds,y,w,loss.function) {
  test.fractions = seq(0.6,1,by=0.02)
  evaluate.loss = function(test.fraction) {
    preds = blended.preds * (1 - test.fraction) + new.preds * test.fraction
    loss = loss.function(preds, y, w)
    loss
  }
  #losses = sapply(test.fractions, evaluate.loss)
  
  #best.fraction = test.fractions[which.min(losses)]
  
  library(nloptr)
  result = bobyqa(0.5,evaluate.loss,lower = 0,upper = 1,control = list(xtol_rel = 0.001))
  best.fraction = result$par
  best.loss     = result$value
  
  return(best.fraction)
}