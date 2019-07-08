library(MatCorR); library(Metabase)

setwd(dirname(parent.frame(2)$ofile))

hdl = new.env()
load('../../../../hdl/apps/app/data/data.rda', envir = hdl)
mcb = new.env()
load('../../app/data/data.rda', envir = mcb)

data = list(
    fct = hdl$data$data$fct,
    mcb = mcb$data$data$mcb,
    bga = mcb$data$data$bga,
    bac = mcb$data$data$bac,
    sfa = mcb$data$data$sfa
)

rm(hdl); rm(mcb)

corr_mcb = lapply(data$mcb, function(li){
    x
})
