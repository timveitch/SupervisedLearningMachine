GbmInteractionDetector <- function(configured.gbm.trainer) {
  function(df.x,y,w,support) {
    output  = configured.gbm.trainer(df.x,y,w,support)
    print("Aca")
    data = if (!is.null(output$data.generator)) {
      output$data.generator(df.x)
    } else {
      df.x
    }
    interact.gbm(output$model,data,1:ncol(data))
  }
}