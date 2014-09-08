# standard includes
initial.wd = getwd()

dir = library.dir <- "~/Documents/Kaggle/Library/R/src"
setwd(library.dir)

utils <- c("LossFunctions.R","FoldsLists.R","InteractionDetection.R")
utils <- c(utils, "NAs.R","Partitioning.R","VariableImportance.R")
utils <- c(utils,"TestUtils.R","MarkdownUtils.R")
utils <- file.path("Utils",utils)

bts <- c("GBMTrainer.R","GBMCVTrainer.R","RandomForestTrainer.R")
bts <- c(bts, "FlexibleRegression.R","GAMTrainer.R","GlmnetTrainer.R")
bts <- c(bts, "GlmnetMarkdown.R")
bts <- file.path("BaseTrainers",bts)

ats <- c("IterativeTrainer.R","CVTrainer.R","SegmentedTrainer.R")
ats <- file.path("AugmentedTrainers",ats)

ens <- c("CVEnsembler.R","EnsembleTrainer.R","IncrementalEnsembler.R")
ens <- c(ens, "ParameterEnsembler.R")
ens <- file.path("Ensembling",ens)

fe <- c("DataGeneratingTrainer.R","DataSelector.R","PrincipalComponentsTrainer.R")
fe <- c(fe,"StepWiseTrainer.R","WindsoringTrainer.R")
fe <- file.path("FeatureEngineering",fe)

hpt <- c("GBMTuner.R","HyperParameterTuner.R","ParameterSelectingTrainer.R")
hpt <- file.path("HyperParameterTuning", hpt)

files <- c(utils,bts,ats,fe,hpt,ens)

for (file in files) {
  print(file)
  source(file)
}

setwd(initial.wd)