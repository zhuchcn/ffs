mcb = new.env()
load("../../../Rdata/mcb_precalc.Rdata", envir = mcb)
pcr = new.env()
load("../../../Rdata/pcr_precalc.Rdata", envir = pcr)
bga = new.env()
load("../../../Rdata/bga_precalc.Rdata", envir = bga)
bac = new.env()
load("../../../Rdata/bac_precalc.Rdata", envir = bac)
sfa = new.env()
load("../../../Rdata/sfa_precalc.Rdata", envir = sfa)

data = list(
    data = list(
        mcb = mcb$mcb,
        pcr = pcr$pcr,
        bga = bga$bga,
        bac = bac$bac,
        sfa = sfa$sfa,
        cli = mcb$cli,
        diet = mcb$diet,
        tree = mcb$tree
    ),
    diff = list(
        mcb = mcb$lm,
        pcr = pcr$ds,
        bga = bga$limma_table,
        bac = bac$limma_list,
        sfa = sfa$limma_table
    ),
    corr = list(
        mcb = list(
            bga = mcb$corr_bga,
            bac = mcb$corr_bac,
            sfa = mcb$corr_sfa,
            cli = mcb$corr_cli,
            diet = mcb$corr_diet
        ),
        pcr = list(
            bga = pcr$corr_bga,
            bac = pcr$corr_bac,
            sfa = pcr$corr_sfa,
            cli = mcb$corr_sfa,
            diet = mcb$corr_diet
        ),
        bga = list(
            sfa = bga$corr_sfa
        ),
        bac = list(
            sfa = bac$corr_sfa
        )
    )
)

save(data, file = "data.rda")