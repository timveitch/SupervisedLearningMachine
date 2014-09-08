ReplaceNasInDFWith <- function(df, replacement) {
  for (col in 1:ncol(df)) {
    na.rows = which(is.na(df[,col]))
    df[na.rows,col] = replacement
  }
  df
}