library(gam)
GamTrainer <- function(params) {
  function(df.x,y,w,support) {
    df = params[["df"]]
    
    family = if (!is.null(params$family)) { 
      params$family 
    } else { 
      "gaussian"
    }
    
    data = cbind(y,df.x)
    x.cols <- colnames(df.x)
    
    names(data)[1] = "target"
    
    num.unique.vals = sapply(x.cols, function(col) {
      length(unique(df.x[,col]))
    })
    regular.var.rows = which(num.unique.vals %in% 2:3)
    regular.vars = if (length(regular.var.rows) > 0) {
      x.cols[regular.var.rows]
    } else { c() }
    spline.var.rows = which(num.unique.vals > 3)
    spline.vars = if (length(spline.var.rows) > 0) {
      x.cols[spline.var.rows]
    } else { c() }
    
    x.formula.spline <- paste("s(",spline.vars,",df=",df,")",collapse = "+",sep = "")
    x.formula.regular <- paste(regular.vars, collapse = "+")
    x.formula = paste(x.formula.spline, x.formula.regular, collapse = "+")
    formula = as.formula(paste("target ~", x.formula))
    
    model = gam(formula, family = family, data = data, weights = w)
    
    predictor = function(newx) {
      predict(model,newx,type = "response")
    }
    list(model = model, predict = predictor)
  }
}