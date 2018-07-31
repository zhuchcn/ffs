pkgs = c("dplyr", "reshape2", "tibble", "Metabase", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/hdl/pre-calc")
## -------- load data ----------------------------------------------------------
load("../../data/hdl.Rdata")
## -------- limma --------------------------------------------------------------
design = model.matrix(data=as(sample_table(HDL_Function), "data.frame"), 
                      ~Treatment * Timepoint + Subject + 1)

limma_list = lapply(Glycopeptide, function(data){
    mSet_limma(data, design, coef = 13, p.value = 13)
})
## -------- correlation --------------------------------------------------------
design2 = model.matrix(data=as(sample_table(HDL_Function), "data.frame"), 
                       ~Subject + 1)
corr_func = function(covar){
    lapply(Glycopeptide, function(glc){
        MatCorPack(X = conc_table(covar), Y = conc_table(glc), 
                   design = design2)
    })
}
corr_fct = corr_func(HDL_Function)
corr_diet = corr_func(Diet)
corr_clinical = corr_func(Clinical)
## -------- save ---------------------------------------------------------------
save(Glycopeptide, HDL_Function, Clinical, Diet,
     limma_list, corr_fct, corr_diet, corr_clinical,
     file = "../Rdata/glc_precalc_new.Rdata")
