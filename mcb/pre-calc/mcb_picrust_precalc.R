## ----- library -----
library(dplyr);library(plyr);library(stringr);library(tibble)
library(data.table); library(limma)
## ----- load data -----
rm(list=ls())
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/microbiome_and_metabolome/")
load("../data/microbiome_and_metabolome.Rdata")
pdata = microbiome$pdata

## ----- read picrust data -----
picrust = lapply(1:3, function(l){
    path = str_c("picrust/seqtab_tax_rarified_norm_ko_l",l,".tsv")
    data = read.delim(path, sep="\t", header=T, skip=1, comment.char = "",
                      stringsAsFactors = F) %>%
        column_to_rownames("X.OTU.ID")
    data = data[,pdata$Description]
    colnames(data) = rownames(pdata)
    return(data)
})
names(picrust) = c("level1","level2","level3")

## ----- linear model -----
design = model.matrix(data=pdata, ~Treatment + Timepoint + Treatment*Timepoint + StudyID + 1)
limma_list = lapply(picrust, function(data){
    fit = lmFit(log2(as.matrix(data)+1), design)
    fit_ebayes = eBayes(fit)
    fit_top = topTable(fit_ebayes, coef = 13, number = nrow(data), 
                       sort.by = "none", p.value=13)
    return(fit_top)
})

## ----- save -----
save(picrust, limma_list, pdata,
     file = "Rdata/mcb_picrust.Rdata")