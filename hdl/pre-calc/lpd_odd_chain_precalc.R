## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"matrixStats","limma")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## ---------------------- load data -------------------------
rm(list=ls())
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/hdl_structure_and_function/')
load('Rdata/lpd_precalc.Rdata')

## ------------------- make odd chain data ----------------------
fdata = mutate(fdata, chain_length = as.integer(
    str_replace(
        str_extract(Annotation, "\\d\\d\\:\\d"),
        "\\:\\d","")))

# unit: mmol/ml or mol/L (M) 
odd_chain_mol = sapply(edata_list$species$Concentration, function(col) 
    col/fdata$mol_wt * 1000) %>% 
    as.data.frame%>%
    mutate(
        feature = fdata$Annotation,
        class = fdata$class) %>%
    filter(fdata$chain_length %% 2 == 1) %>%
    melt(id.var = c("feature", "class"),
         variable.name = "sample_id", value.name = "conc") %>%
    ddply(.(class, sample_id), summarize,
          conc = sum(conc)) %>%
    dcast(class~sample_id, value.var = "conc") %>%
    column_to_rownames("class") %>%
    t %>% as.data.frame %>% 
    rownames_to_column("sample_id") %>%
    mutate(overall = select(., -sample_id) %>% rowSums()) %>% 
    column_to_rownames("sample_id") 

odd_chain_prop = edata_list$species$Proportion %>%
    as.data.frame%>%
    mutate(
        feature = fdata$Annotation,
        class = fdata$class) %>%
    filter(fdata$chain_length %% 2 == 1) %>%
    melt(id.var = c("feature", "class"),
         variable.name = "sample_id", value.name = "conc") %>%
    ddply(.(class, sample_id), summarize,
          conc = sum(conc)) %>%
    dcast(class~sample_id, value.var = "conc") %>%
    column_to_rownames("class") %>%
    t %>% as.data.frame %>% 
    rownames_to_column("sample_id") %>%
    mutate(overall = select(., -sample_id) %>% rowSums()) %>% 
    column_to_rownames("sample_id") 

odd_chain_list = list(
    concentration = odd_chain_mol,
    proportion = odd_chain_prop
)


## --------------------- linear model -----------------------
design = model.matrix(data=pdata, ~TX + Day + TX*Day + Subj + 1)
lmfit = function(data, design){
    design = design[,-1]
    result = sapply(data, function(col){
        fit = lm (col ~ design)
        fit = summary(fit)
        return(fit$coefficients[13,])
    }) %>% 
        t %>%
        as.data.frame
    return(result)
}
fit = lapply(odd_chain_list, function(data)
    lmfit(data, design))

## ----------------- correlation function ----------------------

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
        return(fit)
    })
    names(out) = colnames(covar_mat)
    return(out)
}

fct_data = clinical_data[,c(5,6,17,18,20,30,31)]
oc_fct_corr = lapply(odd_chain_list, function(data)
    runCorrTestsforAllVariable(t(data), fct_data))

oc_diet_corr = lapply(odd_chain_list, function(data)
    runCorrTestsforAllVariable(t(data), diet_data))

## ---------------------- save --------------------------
Odd_Chain = list(
    oc_data = odd_chain_list,
    lm = fit,
    fct_corr = oc_fct_corr,
    diet_corr = oc_diet_corr
)
save(Odd_Chain, file = "Rdata/lpd_odd_chain_precalc.Rdata" )


