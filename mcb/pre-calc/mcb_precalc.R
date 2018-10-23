## -------- packages -----------------------------------------------------------
pkgs = c("dplyr", "stringr", "reshape2", "tibble", "phyloseq", "phylox", 
         "Metabase", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("../../data/mcb.Rdata")
## -------- summarize ----------------------------------------------------------
mcb = as_phyloseq(mcb)
mcb = phylox::fix_duplicate_tax(mcb)
mcb_count = summarizeFromPhyloseq(mcb) %>%
    as_MicrobiomeSetList()
mcb_prop = transform_sample_counts(mcb, function(x) x/sum(x, na.rm = TRUE)) %>%
    summarizeFromPhyloseq() %>%
    as_MicrobiomeSetList()

mcb = list(
    count = mcb_count,
    proportion = mcb_prop
)

mcb = lapply(
    mcb, function(li){
        lapply(li, function(mset) 
               subset_features(mset, featureNames(mset) != "NA"))
    }
)

## -------- stats --------------------------------------------------------------
design = model.matrix(data = as(mcb$count$kingdom$sample_table, "data.frame"), 
                      ~Treatment*Timepoint + Subject)
ds = lapply(mcb$count, function(x) mSet_deseq(x, design, 13))
lm = lapply(mcb$proportion, function(x) mSet_limma(x, design, coef = 13, p.value = 13))
lm = list(
    deseq2 = ds,
    limma = lm
)
## -------- correlation --------------------------------------------------------
design2 = model.matrix(data=as(bga$sample_table, "data.frame"), 
                       ~Subject + 1)

corr_func = function(covar){
    message(substitute(covar))
    lapply(mcb, function(norm){
        li = lapply(norm, function(lvl){
            message("processing...")
            MatCorPack(X = covar$conc_table, Y = lvl$conc_table,  design = design2)
        })
        names(li) = names(norm)
        return(li)
    })
}

corr_bga = corr_func(bga)
corr_bac = corr_func(bac)
corr_sfa = corr_func(sfa)
corr_cli = corr_func(cli)
corr_diet = corr_func(diet)

## -------- save ---------------------------------------------------------------
save(mcb, bga, bac, sfa, cli, diet, tree,
     lm, corr_bga, corr_bac, corr_sfa, corr_cli, corr_diet,
     file = "../Rdata/mcb_precalc.Rdata")
