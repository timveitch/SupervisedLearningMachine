DataGeneratedTrainer <- function(data.generator) {
  function(trainer) {
    function(df.x,y,w,support) {
      x.generated = data.generator(df.x)
      output = trainer(x.generated,y,w,support)
      wrap.func = WrapModelWithDataGenerator(data.generator)
      ApplyToFinalAndCVOutputs(output,wrap.func)
    }
  }
}

WrapModelWithDataGenerator <- function(data.generator) {
  function(model.output) {
    result = model.output
    predictor = function(newx) {
      model.output$predict(data.generator(newx))
    }
    result$predict        = predictor
    result$data.generator = data.generator
    
    result
  }
}