## -------- packages -----------------------------------------------------------
pkgs = c("dplyr", "stringr", "reshape2", "tibble", "Metabase", "MatCorR",
         "phyloseq", "phylox", "DESeq2")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
load("../../data/mcb.Rdata")
## -------- read picrust data --------------------------------------------------
picrust = lapply(1:3, function(l){
    path = str_c("../picrust/seqtab_tax_rarified_norm_ko_l",l,".tsv")
    data = read.delim(path, sep="\t", header=T, skip=1, comment.char = "",
                      stringsAsFactors = F) %>%
        column_to_rownames("X.OTU.ID")
    data = data[,sample_data(mcb)$Description]
    colnames(data) = sample_names(mcb)
    return(data)
})
names(picrust) = c("level1","level2","level3")
pct = lapply(picrust, function(lvl){
    Metabase::MultxSet(
        conc_table = conc_table(as.matrix(lvl)), 
        sample_table = sample_table(
            sample_data(mcb)[,c("Treatment", "Timepoint", "Subject")]
        ),
        experiment_data = MultiExperimentData(experiment_type = "Picrust Predicted Function")
    )
})
## -------- deseq2 -------------------------------------------------------------
design = model.matrix(data=as(sample_data(mcb), "data.frame"), 
                      ~Treatment + Timepoint + Treatment*Timepoint + Subject + 1)
run_deseq = function(object, design, coef){
    de = DESeqDataSetFromMatrix(as(object$conc_table, "matrix"), 
                                as(object$sample_table, "data.frame"), 
                                design = design)
    de = DESeq(de)
    res = results(de, name = resultsNames(de)[coef])
    res = as.data.frame(res)
    res = res %>%
        rownames_to_column("feature") %>%
        mutate(
        baseMean = ifelse(is.na(baseMean), 0, baseMean),
        log2FoldChange = ifelse(is.na(log2FoldChange), 0, log2FoldChange),
        lfcSE = ifelse(is.na(lfcSE), 0, lfcSE),
        stat = ifelse(is.na(stat), 0, stat),
        pvalue = ifelse(is.na(pvalue), 1, pvalue),
        padj = ifelse(is.na(padj), 1, padj)
    ) %>%
        column_to_rownames("feature")
    return(res)
}
ds_list = lapply(pct, function(lvl) run_deseq(lvl, design, 13))

## -------- correlation --------------------------------------------------------
design2 = model.matrix(data=as(bga$sample_table, "data.frame"), 
                       ~Subject + 1)
corr_func = function(covar){
    message(substitute(covar))
    lapply(pct, function(lvl){
        message("I'm working...")
        MatCorPack(X = conc_table(covar), Y = conc_table(lvl), 
                   design = design2)
    })
}
corr_bga = corr_func(bga)
corr_sfa = corr_func(sfa)
corr_cli = corr_func(cli)
corr_diet = corr_func(diet)

save(picrust, limma_list, pdata,
     file = "Rdata/mcb_picrust.Rdata")