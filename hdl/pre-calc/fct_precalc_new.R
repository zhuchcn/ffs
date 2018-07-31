pkgs = c("dplyr", "reshape2", "tibble", "Metabase", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/hdl/pre-calc")
## -------- load data ----------------------------------------------------------
load("../../data/hdl.Rdata")
## --------------------- limma ----------------------------
design = model.matrix(
    data = as(sample_table(HDL_Function), "data.frame"),
    ~ Treatment * Timepoint + Subject + 1
)
limma_result = mSet_limma(HDL_Function, design, coef = 13, p.value = 13)

## ----------------------- corr  --------------------------
methods = c("pearson", "spearman", "kendall", "lm")
design2 = model.matrix(data = as(sample_table(HDL_Function), "data.frame"), 
                       ~Subject + 1)

corr_fct = MatCorPack(X=conc_table(HDL_Function), Y=conc_table(HDL_Function),
                      methods = methods, design = design2)
corr_clinical = MatCorPack(X=conc_table(Clinical), Y=conc_table(HDL_Function),
                           methods = methods, design = design2)
## -------- save ---------------------------------------------------------------
save(HDL_Function, Clinical, limma_result, corr_fct, corr_clinical,
     file = "../Rdata/fct_precalc_new.Rdata")
