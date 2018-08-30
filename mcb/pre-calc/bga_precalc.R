pkgs = c('dplyr','stringr','reshape2','tibble', 'limma','Metabase', 'MatCorR')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## -------- load data ----------------------------------------------------------
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/mcb/pre-calc")
load('../../data/mcb.Rdata')

## -------- limma --------------------------------------------------------------
design = model.matrix(data=as(bga$sample_table, "data.frame"), 
                      ~Treatment * Timepoint + Subject + 1)
limma_table = mSet_limma(bga, design, coef = 13, p.value = 13)

## -------- corr ---------------------------------------------------------------
design2 = model.matrix(data=as(bga$sample_table, "data.frame"), 
                       ~Subject + 1)

corr_sfa = MatCorPack(X = sfa$conc_table, Y = bga$conc_table, design = design2)

## ------------------------- save ------------------------
save(bga, limma_table, corr_sfa, sfa,
     file = "../Rdata/bga_precalc.Rdata")
