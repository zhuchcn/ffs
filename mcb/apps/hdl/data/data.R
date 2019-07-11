setwd(dirname(parent.frame(2)$ofile))

hdl = new.env()
load('../../../../hdl/apps/app/data/data.rda', envir = hdl)
mcb = new.env()
load('../../app/data/data.rda', envir = mcb)

data = list(
    fct = hdl$data$data$fct,
    lpd = list(
        class = hdl$data$data$lpd$class$Proportion,
        species = hdl$data$data$lpd$feature$Proportion,
        eod = hdl$data$data$lpd$summarize$EOD18,
        acl = hdl$data$data$lpd$summarize$ACL,
        odd = hdl$data$data$lpd$summarize$`Odd Chain`,
        ratios = hdl$data$data$lpd$summarize$`Lipid Ratios`
    ),
    glc = hdl$data$data$glc,
    cli = hdl$data$data$cli,
    mcb = mcb$data$data$mcb,
    bga = mcb$data$data$bga,
    bac = mcb$data$data$bac,
    sfa = mcb$data$data$sfa
)

rm(hdl); rm(mcb)

save(data, file = "data.rda")
