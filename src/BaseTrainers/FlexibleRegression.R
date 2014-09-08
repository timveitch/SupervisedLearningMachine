FlexibleRegression <- function(response.function, loss.function, starting.parameters) {
  function(df.x,y,w,support) {
    lambda = 1
    reg.loss.function = function(lambda,parameters) {
      function(preds,y,w) {
        loss.function(preds,y,w) +  lambda * sum(lambda*parameters^2)
      }
    }
    
    evaluate.loss <- function(parameters) {
      #print(parameters)
      fit = response.function(parameters,df.x)
      
      loss = reg.loss.function(lambda, parameters)(fit,y,w)
      #print(loss)
      #print("Parameters:", paste(parameters, sep =","))
      #print("Loss:", loss)
      loss
    }
    
    library(nloptr)
    #print(starting.parameters)
    #print(evaluate.loss(starting.parameters))
    starting.parameters = sapply(starting.parameters, function(p) { rnorm(1) })
    result = bobyqa(starting.parameters,evaluate.loss,control = list(xtol_rel = 0.001))
    best.params = result$par
    best.loss   = result$value
    print(paste("Best params", best.params))
    print(paste("Best loss", best.loss))
    
    predictor = function(new.x) {
      response.function(best.params,new.x)
    }
    list(predict = predictor, model = best.params)
  }
}

