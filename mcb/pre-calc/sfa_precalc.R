pkgs = c('dplyr','stringr','reshape2','tibble', 'limma','Metabase')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## -------- load data ----------------------------------------------------------
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/mcb/pre-calc")
load('../../data/mcb.Rdata')

## -------- limma --------------------------------------------------------------
design = model.matrix(data=as(sfa$sample_table, "data.frame"), 
                      ~Treatment * Timepoint + Subject + 1)
limma_table = mSet_limma(sfa, design, coef = 13, p.value = 13)

## ------------------------- save ------------------------
save(sfa, limma_table,
     file = "../Rdata/sfa_precalc.Rdata")
