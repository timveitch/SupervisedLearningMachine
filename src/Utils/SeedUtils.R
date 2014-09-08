FoldSeed <- function(base.seed,repeatnr,foldnr) {
  base.seed * 10000 + 100*repeatnr + (foldnr+1)
}