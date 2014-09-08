WindsoringTrainer <- function(beta) {
  function(trainer) {
    function(df.x,y,w,support) {
      # for each continuous var, identify the "bounds" within the
      # beta and 1-beta quantiles.  Store these in a list.
      bounds = list()
      
      for (colname in colnames(df.x)) {
        if (is.factor(df.x[,colname])) { next }
        
        b = list()
        
        betaVal = 1 # DO IT...
        oneMinusBetaVal = 1 # DO IT...
        
        b$var = colname
        b$lower = betaVal
        b$upper = oneMinusBetaVal
        bounds[[colname]] = b
      }
      
      data.generator = function(new.x) {
        mod.x = new.x
        for (b in bounds) {
          var   = b$var
          lower = b$lower
          upper = b$upper
          mod.x[new.x[,var] > upper,var] = upper
          mod.x[new.x[,var] < lower,var] = lower
        }  
        mod.x
      }
      DataGeneratedTrainer(data.generator)(trainer)(df.x,y,w,support)
      
    }
  }
}