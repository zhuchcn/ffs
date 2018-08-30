## -------- packages -----------------------------------------------------------
pkgs = c("dplyr", "stringr", "reshape2", "tibble", "Metabase", "MatCorR",
         "phyloseq", "phylox", "DESeq2")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("../../data/mcb.Rdata")
## -------- stats --------------------------------------------------------------
design = model.matrix(data = as(pcr$l1$sample_table, "data.frame"), 
                      ~Treatment*Timepoint + Subject)
ds = lapply(pcr, function(x) mSet_deseq(x, design, 13))
## -------- correlation --------------------------------------------------------
design2 = model.matrix(data=as(bga$sample_table, "data.frame"), 
                       ~Subject + 1)

corr_func = function(covar){
    message(substitute(covar))
    li = lapply(pcr, function(lvl){
        message("processing...")
        MatCorPack(X = covar$conc_table, Y = lvl$conc_table,  design = design2)
    })
    names(li) = names(norm)
    return(li)
}

corr_bga = corr_func(bga)
corr_sfa = corr_func(sfa)
corr_cli = corr_func(cli)
corr_diet = corr_func(diet)

## -------- save ---------------------------------------------------------------
save(pcr, bga, sfa, cli, diet, ds, corr_bga, corr_sfa, corr_cli, corr_diet,
     file = "../Rdata/pcr_precalc.Rdata")
