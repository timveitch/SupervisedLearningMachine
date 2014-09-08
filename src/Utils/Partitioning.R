PartitionVariable <- function(x,y,w) {
  na.rows = which(is.na(x))
  #print(na.rows)
  df = data.frame(x = x, y = y, w = w)
  #print(df$x)
  df.no.na = if (length(na.rows) > 0) {
    df[-na.rows,,drop = F]
  } else { df }
  
  #print(df.no.na$x)
  df.no.na = df.no.na[order(df.no.na$x),]
  BestSplit(df.no.na)
}

BestSplit <- function(df) {
  wy = df$y * df$w
  
  leftSumW   = cumsum(df$w)
  leftSumWY  = cumsum(wy)
  
  rightSumW  = sum(df$w)  - leftSumW
  rightSumWY = sum(wy)    - leftSumWY
  
  result = leftSumWY^2 / leftSumW + rightSumWY^2 / rightSumW
  prev.x = df$x[1]
  best.result = -Inf
  split.x = NULL
  for (i in 2:nrow(df)) {
    new.x = df[i,1]
    if (new.x != prev.x) {
      
      test.result = result[i-1]
      if (test.result > best.result) {
        best.result = test.result
        split.x = (prev.x + new.x) / 2
      }
      
      prev.x = new.x
    }
  }
  split.x
}
