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

## ------------------- make ratio data ----------------------
lpd_ratio_data = edata_list$class$Concentration %>%
    t %>%
    as.data.frame %>%
    rownames_to_column("Sample_ID") %>% 
    mutate(`CE/FC` = CE / Cholesterol,
           `PC/LPC` = PC / LPC,
           `TG/DG` = TG / DG,
           `SM/Cer` = SM / Ceramide,
           `PC/FC` = PC/Cholesterol,
           `TG/CE` = TG/CE,
           `Surface/Core` = (Ceramide+Cholesterol+DG+LPC+PC+PE+SM)/(CE+TG)) %>%
    select(c("Sample_ID","CE/FC","PC/LPC", "TG/DG", "SM/Cer", "PC/FC","TG/CE","Surface/Core")) %>% 
    column_to_rownames("Sample_ID")

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
fit = lmfit(lpd_ratio_data, design)

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

covar_data = clinical_data[,-which(colnames(clinical_data)%in%c("Gender Code", "Gender" ,"Calorie Level","Gender-Specific HDL-C Clinical Cut-Off","HDL TNF-a Responder/Non-Responder"))]
ratio_clinical_corr = runCorrTestsforAllVariable(t(lpd_ratio_data), covar_data[,-(1:4)])

ratio_diet_corr = runCorrTestsforAllVariable(t(lpd_ratio_data), diet_data)

## ---------------------- save --------------------------
lpd_ratio = list(
    lpd_ratio_data = lpd_ratio_data,
    lm = fit,
    clinical_corr = ratio_clinical_corr,
    diet_corr = ratio_diet_corr
)
save(lpd_ratio, file = "Rdata/lpd_ratio_precalc.Rdata" )


