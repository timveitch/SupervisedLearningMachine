ForwardStepwiseVariableSelector <- function(test.trainer, folds.list.generator, loss.function, quit.after.n.fails, seed, step.back.each = F, step.back.final = T, final.trainer = NULL) {
  function(warm.start.vars = c()) {
    function(df.x,y,w,support) {
      vars = colnames(df.x)
      every.var.each.step = lapply(vars, function(var) {
        vars
      })
      
      try.failed.vars = (quit.after.n.fails > 1)
  
      stepwise.model <- ForwardStepwiseCustomVariableSelector(test.trainer, every.var.each.step, folds.list.generator, loss.function, try.failed.vars, quit.after.n.fails, seed, step.back.each, step.back.final, final.trainer)
      stepwise.model(warm.start.vars)(df.x,y,w,support)
      
    }
  }
}

ForwardStepwiseOrderedVariableSelector <- function(test.trainer, folds.list.generator, loss.function, quit.after.n.fails, seed, step.back.final = F, final.trainer = NULL) {
  function(variable.order, warm.start.vars = c()) {
    function(df.x,y,w,support) {
      vars = colnames(df.x)
      
      single.var.each.step = lapply(variable.order, function(var) {
        var
      })
      
      try.failed.vars = F
      step.back.each  = F
      
      stepwise.model <- ForwardStepwiseCustomVariableSelector(test.trainer, single.var.each.step, folds.list.generator, loss.function, try.failed.vars, quit.after.n.fails, seed, step.back.each, step.back.final, final.trainer)
      stepwise.model(warm.start.vars)(df.x,y,w,support)
    }
  }
}

InitialState <- function() {
  state                    = list()
  state$included.vars      = c()
  state$best.included.vars = c()
  state$best.loss          = Inf
  state$best.output        = NULL
  state$quit.count         = 0
  state
}

WarmStart = function(warm.start.vars,test.trainer,folds.list.generator,loss.function,seed) {
  function(df.x,y,w,support) {
    state = InitialState()
    print("Testing warm start")
    
    warm.start.vars = intersect(warm.start.vars, colnames(df.x))
    
    if (length(warm.start.vars) == 0) {
      return(state)
    }
    
    state$included.vars      = warm.start.vars
    state$best.included.vars = warm.start.vars
    
    data.generator = MakeDataGenerator(warm.start.vars)
    trainer        = DataGeneratedTrainer(data.generator)(test.trainer)
    
    state$best.output = TrainModelWithCV(trainer, folds.list.generator, seed)(df.x,y,w,support)
    state$best.loss   = loss.function(state$best.output$cv.preds, y, w)
    
    print(paste("Warm start loss", state$best.loss))
    state
  }
}

TestAddition <- function(vars,state,try.failed.vars,test.trainer,folds.list.generator,loss.function,seed) {
  function(df.x,y,w,support) {
    print("Testing addition")
  
    new.vars = setdiff(vars, state$included.vars)
    if (length(new.vars) == 0) { return(state) }
    
    var.sets.list = lapply(new.vars, function(new.var) {
      union(state$included.vars,new.var)
    })
    test.func = TestVarSets(var.sets.list,state,test.trainer,try.failed.vars,loss.function,folds.list.generator,seed)
    test.func(df.x,y,w,support)
  }
}

TestRemoval = function(vars.to.remove,state,try.failed.vars,test.trainer,folds.list.generator,loss.function,seed) {
  print("Testing removal")
  vars.to.remove = intersect(vars.to.remove,state$included.vars)
  
  if (length(vars.to.remove) == 0) { return(state) }
  var.sets.list = lapply(vars.to.remove, function(var) {
    setdiff(state$included.vars,var)
  })
  
  test.func = TestVarSets(var.sets.list,state,test.trainer,try.failed.vars,loss.function,folds.list.generator,seed)
  test.func(df.x,y,w,support)
}

TestVarSets <- function(var.sets.list,state,test.trainer,try.failed.vars,loss.function,folds.list.generator,seed) {
  function(df.x,y,w,support) {
    print("aca")
    data.generators.list = lapply(var.sets.list, function(vars) {
      MakeDataGenerator(vars)
    })
    
    selector.func = DataSelector(test.trainer,data.generators.list,loss.function,folds.list.generator,seed)
    best.test.output = selector.func(df.x,y,w,support)
    test.loss = best.test.output$cv.loss
    best.test.vars = var.sets.list[[which.min(best.test.output$individual.losses)]]
    
    if (test.loss < state$best.loss) {
      # removal is good!
      #var.to.remove = vars.to.remove[which.min(best.test.output$individual.losses)]
      #included.vars = included.vars[included.vars != var.to.remove]
      print("Improvement! Best Vars:")
      print(paste(best.test.vars,collapse = ","))
      
      state$included.vars = best.test.vars
      state$best.included.vars = state$included.vars
      state$best.loss   = test.loss
      state$best.output = best.test.output
      state$quit.count  = 0
      
    } else if (try.failed.vars) {
      print(paste("Included var for further testing:",best.test.var))
      state$included.vars <- best.test.vars
      state$quit.count = state$quit.count + 1
      state$just.modified = T
    } else {
      state$quit.count = state$quit.count + 1
      state$just.modified = F
    }
    state
  }
}

ForwardStepwiseCustomVariableSelector <- function(test.trainer,variables.to.consider.per.step, folds.list.generator, loss.function, try.failed.vars, quit.after.n.fails, seed, step.back.each = F, step.back.final = F, final.trainer = NULL) {
  function(warm.start.vars = c()) {
    function(df.x,y,w,support) {
      state = if (length(warm.start.vars) > 0) {
        warm.start.func = WarmStart(warm.start.vars,test.trainer,folds.list.generator,loss.function,seed)
        warm.start.func(df.x,y,w,support)
      } else {
        InitialState()
      }

      for (variables.to.consider in variables.to.consider.per.step) {
        if (state$quit.count == quit.after.n.fails) { 
          break
        }
        adding.func = TestAddition(variables.to.consider,state,try.failed.vars,test.trainer,folds.list.generator,loss.function,seed)
        state = adding.func(df.x,y,w,support)
      
        if (step.back.each && state$just.modified && length(state$included.vars) > 1) {
          vars.to.remove = included.vars[1:(length(included.vars)-1)]
          removal.func = TestRemoval(vars.to.remove,state,F,test.trainer,folds.list.generator,loss.function,seed)
          state = removal.func(df.x,y,w,support)
        }
      }
      if (step.back.final && length(state$included.vars > 1)) {
        state$included.vars = state$best.included.vars
        state$just.modified = T
        
        while (state$just.modified) {
          vars.to.remove = included.vars[1:(length(included.vars)-1)]
          removal.func = TestRemoval(vars.to.remove,state,F,test.trainer,folds.list.generator,loss.function,seed)
          state = removal.func(df.x,y,w,support)
        }
      }
      
      print("Final variables")
      print(state$best.included.vars)
      final.output = if (!is.null(final.trainer)) {
        data.generator = state$best.output$data.generator
        DataGeneratedTrainer(data.generator)(final.trainer)(df.x,y,w,support)
      } else {
        state$best.output
      }
      final.output$selected.vars = state$best.included.vars
      final.output
    }
  }
}

MakeDataGenerator = function(vars) {
  function(df.x) {
    cols = which(colnames(df.x) %in% vars)
    df.x[,cols,drop=F]
  }
}