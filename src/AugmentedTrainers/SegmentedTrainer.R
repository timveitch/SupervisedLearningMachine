TrainSegmentedModel <- function(trainer,segmentation.var,seed) {
  function(df.x,y,w,support) {
    print(paste("Segmenting on var:",segmentation.var))
    # segmentation var must be in x
    segmentation.column.index = which(colnames(df.x) == segmentation.var)
    if (length(segmentation.column.index) != 1) {
      stop(paste("Expecting exactly one column to have name", segmentation.var))
    }
    segmentation.column = df.x[,segmentation.var]
    
    unique.segments.train = sort(unique(segmentation.column))
    
    output.func = function(segment) {
      rows = which(segmentation.column == segment)
      x.segment = df.x[rows,-segmentation.column.index,drop = F]
      y.segment = y[rows]
      w.segment = w[rows]
      support.segment = if (is.null(support)) { 
        NULL
      } else { 
        support[rows,,drop = F] 
      }
      output = trainer(x.segment,y.segment,w.segment,support.segment)
      output$segment = segment
      output$segmentation.var = segmentation.var
      output
    }
    segment.outputs = list()
    for (segment in unique.segments.train) {
      segment.outputs[[as.character(segment)]] = output.func(segment)
    }
    
    predictor = function(new.x) {
      preds = rep(0, nrow(new.x))
      pred.segmentation.column.index = which(colnames(new.x) == segmentation.var)
      pred.segmentation.column = new.x[,segmentation.var]
      unique.segments.test = sort(unique(pred.segmentation.column))
      new.segments = setdiff(unique.segments.test, unique.segments.train)
      if (length(new.segments) > 0) {
        stop(paste("Segments detected in new.x which weren't present in training data:",paste(new.segments, sep =",")))
      }
      for (segment in unique.segments.test) {
        segment.rows = which(pred.segmentation.column == segment)
        new.x.segment = new.x[segment.rows, -pred.segmentation.column.index, drop = F]
        preds[segment.rows] = segment.outputs[[as.character(segment)]]$predict(new.x.segment)
      }
      preds
    }
    list(predict = predictor, model = segment.outputs)
  }
}