## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"matrixStats","limma", "data.table")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## ---------------------- load data -------------------------
rm(list=ls())
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/hdl_structure_and_function/')
load('Rdata/lpd_precalc.Rdata')
rm(list=c("clinical_corr","diet_corr"))
load('../data/hdl_structure_and_function.Rdata')

## --------------------- diet data --------------------------
diet_data = diet_data[,c(1,3,4,7,8,9,10,12,13,14,15,16,17,18,29,30,31,32,33,34)] %>%
    melt(id.var = c("Subject ID", "Treatment", "Time"),
         variable.name = "Feature",
         value.name = "Value") %>%
    filter(!is.na(Value)) %>%
    mutate(Timepoint = ifelse(
        Time == "Pre", "Baseline", Treatment
    )) %>% 
    dcast(`Subject ID` + Feature ~ Timepoint,
          value.var = "Value") %>% 
    ddply(.(`Subject ID`, Feature), summarise,
          FF = FF - Baseline,
          Med = Med - Baseline) %>% 
    melt(id.var = c("Subject ID", "Feature"),
         variable.name = "TX",
         value.name = "Value") %>% 
    mutate(Sample = str_c(`Subject ID`, TX, sep="_")) %>% 
    dcast(Sample ~ Feature, value.var = "Value") %>%
    column_to_rownames("Sample")
    
## --------------------- edata list --------------------------
clean_edata = function(data, pdata){
    data %>%
        t %>% as.data.frame %>%
        mutate(
            TX = pdata$TX,
            Day = pdata$Day,
            Subj = pdata$Subj
        ) %>% 
        melt(id.var = c("Subj","TX","Day"),
             variable.name = "Feature",
             value.name = "Value") %>% 
        dcast(Subj + TX + Feature ~ Day,
              value.var = "Value") %>%
        ddply(.(Subj, TX, Feature), summarise,
              change = Post - Pre) %>%
        mutate(Sample = str_c(Subj, TX, sep = "_")) %>%
        dcast(Feature ~ Sample, value.var = "change") %>% 
        column_to_rownames("Feature")
}

lpd_data = lapply(edata_list, function(sublist){
    lapply(sublist, function(data){
        clean_edata(data, pdata)
    })
})

## -------------------- Unsatuation ----------------------------
unsat_data = clean_edata(Unsatuation$edata, pdata)

## -------------------- Chain Lenght ----------------------------
cl_data = clean_edata(Chain_Length$edata, pdata)

## ---------------------- function data --------------------------
fct_data = clinical_data[,c(1,3,4,5,6,9,17,18,20,25,26,27,29)] %>%
    melt(id.var = c("Subject ID", "Treatment", "Time"),
         variable.name = "Feature",
         value.name = "Value") %>% 
    dcast(`Subject ID` + Treatment + Feature ~ Time,
          value.var = "Value") %>%
    ddply(.(`Subject ID`, Treatment, Feature), summarise,
          change = Post - Pre) %>% 
    mutate(Sample = str_c(`Subject ID`, Treatment, sep = "_")) %>% 
    dcast(Sample ~ Feature,
          value.var = "change") %>%
    column_to_rownames("Sample")


## ----------------- correlation function ----------------------
#
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

## -------------------- corr lipidome ---------------------------
lpd_corr = lapply(lpd_data, function(sublist){
    lapply(sublist, function(data){
        runCorrTestsforAllVariable(data, diet_data)
    })
})

## -------------------- corr unsatuation ---------------------------
unsat_corr = runCorrTestsforAllVariable(unsat_data, diet_data)

## -------------------- corr chain length ---------------------------
cl_corr = runCorrTestsforAllVariable(cl_data, diet_data)

## -------------------- corr function ---------------------------
fct_corr = runCorrTestsforAllVariable(t(fct_data), diet_data)

## -------------------- make a pdata -------------------------
pdata = str_split(rownames(diet_data), "_", n=2, simplify = T) %>%
    as.data.frame %>%
    setnames(c("Subj","TX")) %>%
    mutate(sample = rownames(diet_data)) %>%
    column_to_rownames("sample")

## --------------------- save -------------------------
save(diet_data, pdata, lpd_data, lpd_corr, unsat_data, unsat_corr, 
     cl_data, cl_corr, fct_data, fct_corr, 
     file = "Rdata/diet_change_precalc.Rdata")
