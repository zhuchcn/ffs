setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/mcb/apps/app/data")

mcb = new.env()
load("../../../Rdata/mcb_precalc.Rdata", envir = mcb)
pcr = new.env()
load("../../../Rdata/pcr_precalc.Rdata", envir = pcr)
bga = new.env()
load("../../../Rdata/bga_precalc.Rdata", envir = bga)
sfa = new.env()
load("../../../Rdata/sfa_precalc.Rdata", envir = sfa)

data = list(
    data = list(
        mcb = mcb$mcb,
        pcr = pcr$pcr,
        bga = bga$bga,
        sfa = sfa$sfa,
        cli = mcb$cli,
        diet = mcb$diet,
        tree = mcb$tree
    ),
    diff = list(
        mcb = mcb$lm,
        pcr = pcr$ds,
        bga = bga$limma_table,
        sfa = sfa$limma_table
    ),
    corr = list(
        mcb = list(
            bga = mcb$corr_bga,
            sfa = mcb$corr_sfa,
            cli = mcb$corr_cli,
            diet = mcb$corr_diet
        ),
        pcr = list(
            bga = pcr$corr_bga,
            sfa = pcr$corr_sfa,
            cli = mcb$corr_sfa,
            diet = mcb$corr_diet
        ),
        bga = list(
            sfa = bga$corr_sfa
        )
    )
)

save(data, file = "data.rda")