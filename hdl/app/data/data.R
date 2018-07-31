setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/hdl/apps/data")

lpd = new.env()
load("../../Rdata/lpd_precalc_new.Rdata", envir = lpd)
glc = new.env()
load("../../Rdata/glc_precalc_new.Rdata", envir = glc)
fct = new.env()
load("../../Rdata/fct_precalc_new.Rdata", envir = fct)
cli = new.env()
load("../../Rdata/cli_precalc_new.Rdata", envir = cli)

data = list(
    data = list(
        lpd = lpd$Lipidome,
        glc = glc$Glycopeptide,
        fct = fct$HDL_Function,
        cli = cli$Clinical,
        diet = lpd$Diet
    ),
    limma = list(
        lpd = lpd$limma_list,
        glc = glc$limma_list,
        fct = fct$limma_result,
        cli = cli$limma_result
    ),
    corr = list(
        lpd = list(
            fct = lpd$corr_fct,
            cli = lpd$corr_clinical,
            diet = lpd$corr_diet
        ),
        glc = list(
            fct = glc$corr_fct,
            cli = glc$corr_clinical,
            diet = glc$corr_diet
        ),
        fct = list(
            fct = fct$corr_fct,
            cli = fct$corr_clinical
        )
    )
)

save(data, file = "data.rda")