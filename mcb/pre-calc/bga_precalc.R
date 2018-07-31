## ------------------- loading librarys ------------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',
         'limma','data.table')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
rm(list=ls())

## ----------------------- load data -----------------------------
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/mcb/')
load('../data/microbiome_and_metabolome.Rdata')

edata = biogenic_amines$edata
pdata = biogenic_amines$pdata
fdata = biogenic_amines$fdata

fdata = fdata[!grepl("iSTD", fdata$Metabolite_Name),]
edata = edata[rownames(fdata),]
rownames(edata) = fdata$Metabolite_Name

## ---------------------- limma ------------------------------
design = model.matrix(data=pdata, ~TX + Day + TX*Day + Subject + 1)
fit = lmFit(edata, design)
fit_ebayes = eBayes(fit)
fit_top = topTable(fit_ebayes, coef=13, number=nrow(edata), sort.by = "none",
                   p.value = 13)

## ---------------------- corr function -------------------------
runCorrTests = function(data, covar){
    
    pearson_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(cor.test(x,y,method='pearson'))
    })
    spearman_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(cor.test(x,y,method='spearman'))
    })
    kendall_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(cor.test(x,y,method='kendall'))
    })
    
    pearson.r = sapply(pearson_list, function(x) x[[4]])
    pearson.p = sapply(pearson_list, function(x) x[[3]])
    spearman.rho = sapply(spearman_list, function(x) x[[4]])
    spearman.p = sapply(spearman_list,function(x) x[[3]])
    kendall.tau = sapply(kendall_list, function(x) x[[4]])
    kendall.p = sapply(kendall_list, function(x) x[[3]])
    
    out.mat = data.frame(
        pearson.r = pearson.r,
        pearson.pvalue = pearson.p,
        spearman.rho = spearman.rho,
        spearman.pvalue = spearman.p,
        kendall.tau = kendall.tau,
        kendall.pvalue = kendall.p,
        row.names = rownames(data)
    )
    return(out.mat)
}

runCorrTestsforAllVariable = function(data,covar_mat){
    out = lapply(1:ncol(covar_mat), function(i){
        fit = runCorrTests(data, covar_mat[,i])
        print(str_c(i, "    ", names(covar_mat)[i]))
        return(fit)
    })
    names(out) = colnames(covar_mat)
    return(out)
}

## ----------------------- self corr ------------------------
self_corr = runCorrTestsforAllVariable(edata, as.data.frame(t(edata)))

## -------------------------- diet corr --------------------------
diet_data = diet_data[,c(6,10,11,12,13,14,15,16,27,28,29,30,31,32)] %>%
    sapply(function(col) col/diet_data$`Cals (kcal)`) %>%
    as.data.frame %>%
    mutate(sampleid = rownames(diet_data)) %>%
    column_to_rownames("sampleid")

diet_corr = runCorrTestsforAllVariable(edata, diet_data)



## ------------------------- save ------------------------
limma_result = fit_top
save(
    edata, pdata, fdata,
    limma_result, self_corr,
    diet_data, diet_corr,
    file = "Rdata/bga_precalc.Rdata"
)
