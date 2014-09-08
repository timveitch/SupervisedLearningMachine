StratifiedFoldsList = function(num.repeats,num.folds,strata,seed) {
  lapply(1:num.repeats, function(r) {
    set.seed(r * seed)
    folds <- rep(0,length(strata))
    for (v in unique(strata)) {
      matches <- which(strata == v)
      folds[matches] = sample(rep(1:num.folds, length.out = length(matches)))
    }
    folds
  })
}

StratifiedFoldsListGenerator = function(df.x,y,w,support,seed) {
  
}
SimpleFoldsList = function(num.repeats, num.folds, num.rows, seed) {
  lapply(1:num.repeats, function(r) {
    set.seed(r * seed)
    sample(rep(1:num.folds, length.out = num.rows))
  })
}

SimpleFoldsListGenerator = function(num.repeats, num.folds) {
  function(df.x,y,w,support,seed) {
    SimpleFoldsList(num.repeats,num.folds,nrow(df.x),seed)
  }
}