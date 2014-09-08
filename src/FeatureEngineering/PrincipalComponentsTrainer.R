PrincipalComponentTrainer <- function(test.trainer, num.components.sequence, loss.function, folds.list.generator, seed, final.trainer = NULL) {
  function(df.x,y,w,support) {
    princomp.obj = princomp(df.x)
    print("Generated principal components!")
    print(summary(princomp.obj))
    data.generator.list = lapply(num.components.sequence, function(num.components) {
      dummy = num.components # this line shouldn't be necessary, but it is.
      PrincipalComponentsGenerator(princomp.obj,dummy)
    })
    
    print("Running data selection for sequence:")
    print(num.components.sequence)
    
    selector.func = DataSelector(test.trainer, data.generator.list, loss.function, folds.list.generator, seed)
    best.output = selector.func(df.x,y,w,support)
    
    result = if (!is.null(final.trainer)) {
      data.generator = best.output$data.generator
      output = DataGeneratedTrainer(data.generator)(final.trainer)
      output$individual.losses = best.output$individual.losses
      output
    } else {
      best.output
    }
    result
  }
}

PrincipalComponentsGenerator <- function(princomp.obj, num.comp) {
  function(df.x) {
    predict(princomp.obj, df.x)[,1:num.comp,drop=F]
  }
}