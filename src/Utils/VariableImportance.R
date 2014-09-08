
RFVariableImportance <- function(configured.rf.trainer) {
  function(df.x,y,w,support) {
    output = configured.rf.trainer(df.x,y,w,support)
    output$model
    
    imp <- as.data.frame(importance(output$model))
    imp <- imp[order(-imp),,drop=F]
    imp
  }
}

GBMVariableImportance <- function(configured.gbm.trainer) {
  function(df.x,y,w,support) {
    output  = configured.gbm.trainer(df.x,y,w,support)
    summary(output$model)
  }
}