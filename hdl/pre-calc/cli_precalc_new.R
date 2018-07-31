library(Metabase)
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/hdl/pre-calc")
## -------- load data ----------------------------------------------------------
load("../../data/hdl.Rdata")
## --------------------- limma ----------------------------
design = model.matrix(
    data = as(sample_table(Clinical), "data.frame"),
    ~ Treatment * Timepoint + Subject + 1
)
limma_result = mSet_limma(Clinical, design, coef = 13, p.value = 13)

## -------- save ---------------------------------------------------------------
save(Clinical, limma_result,
     file = "../Rdata/cli_precalc_new.Rdata")
