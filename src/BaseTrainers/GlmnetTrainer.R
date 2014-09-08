TrainCVGlmnet <- function(glmnet.params) {
  library(glmnet)
  function(df.x, y, w, support, seed, parallel) {
    set.seed(seed)
    
    vars    = colnames(df.x)
    formula = as.formula(paste("~", paste(vars, collapse = "+")))
    mm.x    = model.matrix(formula, data = df.x)
    params = ExpandGlmnetParams(glmnet.params,mm.x,y,w)
    params$parallel = parallel
    
    model  = do.call("cv.glmnet", params)
    lambda.min <- model$lambda.min
    
    predictor <- function(new.x) {
      mm.new.x <- model.matrix(formula, data = new.x)
      preds <- predict(model, mm.new.x, s = lambda.min,type = "response")
    }
    
    coefficients <- predict(model, s = lambda.min, type = "coefficients")
    if (rownames(coefficients)[2] == "(Intercept)") {
      coefficients = coefficients[-2,]
    }
    output = model
    output$predict      = predictor
    output$coefficients = coefficients
    output$seed         = seed
    output
  }
}

ExpandGlmnetParams <- function(glmnet.params,x,y,w) {
  exp.params = glmnet.params
  if (is.null(exp.params$nfolds)) {
    exp.params$nfolds = 8
  }
  exp.params$x = x
  exp.params$y = y
  exp.params$w = w
  exp.params
}
