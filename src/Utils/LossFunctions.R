WeightedMSE <- function(preds,actuals,weights) { 
  sum(weights * ((preds - actuals)^2)) / sum(weights)
}

WeightedGini <- function(submission, solution, weights){
  solution   <- as.numeric(as.vector(solution)) # in case solution is a factor
  submission <- as.numeric(as.vector(submission)) # in case submission is a factor
  
  df = data.frame(solution = solution, weights = weights, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = cumsum((df$weights/sum(df$weights)))
  totalPositive <- sum(df$solution * df$weights)
  df$cumPosFound <- cumsum(df$solution * df$weights)
  df$Lorentz <- df$cumPosFound / totalPositive
  n <- nrow(df)
  gini <- sum(df$Lorentz[-1]*df$random[-n]) - sum(df$Lorentz[-n]*df$random[-1])
  return(-gini)
}

NormalizedWeightedGini <- function(submission, solution, weights) {
  - WeightedGini(submission, solution, weights) / WeightedGini(solution, solution, weights)
}