
GBMResponseSurfaceTrainer <- function(folds.list.generator,loss.function,seed) {
  function(df.x,y,w,support) {
    # train gam model
    rf.trainer = RFTrainer(list(ntree = 100))
    
    dfs <- seq(1.0,2.5,by = 0.25)
    gam.params.list = lapply(dfs,function(df) { list(df = df) })    
    gam.trainer = ParameterTrainer(GamTrainer, gam.params.list, loss.function, folds.list.generator,seed)
    
    tree.interval = 10
    max.trees = 1000
    give.up.after.n.intervals = 10
    gbm.params = list(distribution = "gaussian",interaction.depth = 3, n.minobsinnode = 1, shrinkage = 0.01)
    gbm.trainer = TrainGBMWithCV(tree.interval,max.trees,give.up.after.n.intervals,loss.function, folds.list.generator)(gbm.params)
    

    trainers = c(rf.trainer,gbm.trainer) # ,gam.trainer
    best.to.worst = F
    blender  = IncrementalBlendingTrainer(loss.function,best.to.worst)
    
    train.ensemble = TrainEnsemble(trainers,blender,folds.list.generator,seed)
    train.ensemble(df.x,y,w,support)
  }
}

TuneGBM <- function(gbm.cv.trainer, surface.trainer, base.gbm.params, tuning.params.universe, folds.list.generator, loss.function, seed) {
  function(df.x,y,w,support) {
    
    tuner.func = TuneHyperParameters(TunableGBMCVTrainer(gbm.cv.trainer,base.gbm.params), surface.trainer, tuning.params.universe, folds.list.generator, loss.function, seed)
    output = tuner.func(df.x,y,w,support)
  
  }
}

TunableGBMCVTrainer <- function(gbm.cv.trainer,base.params) {
  function(tuning.params) {
    mod.params = base.params
    for (param.name in GBMTuningVars) {
      mod.params[[param.name]] = tuning.params[[param.name]]
    }
    gbm.cv.trainer(mod.params)
  }
}

GBMTuningVars = c("interaction.depth","n.minobsinnode","bag.fraction")