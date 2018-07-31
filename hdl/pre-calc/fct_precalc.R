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
fct_data = clinical_data[,c(5,6,17,18,20,30,31)]
diet_data = diet_data[,c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,21,22,23,24,25,26)]

## -------------------- linear model ---------------------------
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
lm_fit = lmfit(fct_data, design)

## ----------------- correlation function ----------------------
#
runCorrTests = function(data, covar){
    
    my_cor_test = function(x, y, method){
        result = try(cor.test(x,y,method=method), silent = TRUE)
        return(result)
    }
    
    pearson_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(my_cor_test(x,y,method='pearson'))
    })
    spearman_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(my_cor_test(x,y,method='spearman'))
    })
    kendall_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(my_cor_test(x,y,method='kendall'))
    })
    
    pearson.r = sapply(pearson_list, function(x){
        if(class(x) == "try-error"){
            return(NA)
        }
        return(x[[4]])
    })
    pearson.p = sapply(pearson_list, function(x){
        if(class(x) == "try-error"){
            return(NA)
        }
        return(x[[3]])
    })
    spearman.rho = sapply(spearman_list, function(x){
        if(class(x) == "try-error"){
            return(NA)
        }
        return(x[[4]])
    })
    spearman.p = sapply(spearman_list,function(x){
        if(class(x) == "try-error"){
            return(NA)
        }
        return(x[[3]])
    })
    kendall.tau = sapply(kendall_list, function(x){
        if(class(x) == "try-error"){
            return(NA)
        }
        return(x[[4]])
    })
    kendall.p = sapply(kendall_list, function(x){
        if(class(x) == "try-error"){
            return(NA)
        }
        return(x[[3]])
    })
    
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

## ------------------- vs fct data ------------------------
fct_corr = runCorrTestsforAllVariable(as.data.frame(t(fct_data)), fct_data)

## ----------------- vs dietary data ----------------------
diet_corr = runCorrTestsforAllVariable(as.data.frame(t(fct_data)), diet_data)

## ------------------------ save -------------------------------
save(fct_data, diet_data, diet_corr, fct_corr, pdata, lm_fit,
     file="Rdata/fct_precalc.Rdata")
