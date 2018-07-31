## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"matrixStats","limma", "data.table")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## ---------------------- load data -------------------------
rm(list=ls())
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/hdl')
load('../data/hdl_structure_and_function.Rdata')

## ------------------- calculate edata ----------------------
edata = lipidome$edata
pdata = lipidome$pdata
fdata = lipidome$fdata

# pdata = pdata[pdata$Subj != "116",]
# pdata$Subj = factor(pdata$Subj)
# edata = edata[,rownames(pdata)]
# 
# clinical_data = clinical_data[clinical_data$`Subject ID` != 116,]
# diet_data = diet_data[diet_data$`Subject ID` != 116,]

edata_conc = edata
rownames(edata_conc) = fdata$Annotation

edata_prop = sapply(edata, function(col){
    return(col/sum(col))
}) %>% as.data.frame
rownames(edata_prop) = fdata$Annotation

edata_norm = apply(edata, 1, function(col){
    col/clinical_data$`HDL Total Protein`
}) %>% t %>% as.data.frame
rownames(edata_norm) = fdata$Annotation

# Get the lipid class table
edata_class = sapply(edata, function(col){
    tapply(col, fdata$class, sum)
}) %>% as.data.frame

edata_class_prop = sapply(edata_class, function(col){
    return(col/sum(col))
}) %>% as.data.frame
rownames(edata_class_prop) = rownames(edata_class)

edata_class_norm = apply(edata_class, 1, function(col){
    col/clinical_data$`HDL Total Protein`
}) %>% t %>% as.data.frame
rownames(edata_class_norm) = rownames(edata_class)

# Construct the data list
edata_list = list(
    class = list(
        "Concentration" = edata_class,
        "Proportion" = edata_class_prop,
        "Normalized by Total Protein" = edata_class_norm
    ),
    species = list(
        "Concentration" = edata_conc,
        "Proportion" = edata_prop,
        "Normalized by Total Protein" = edata_norm
    )
)

## ----------------------- limma ---------------------------
# limma modeling
design = model.matrix(data=pdata, ~TX + Day + TX*Day + Subj + 1)
runLimma = function(edata, design){
    data = log2(edata+1)
    fit = lmFit(data, design)
    fit_ebayes = eBayes(fit)
    fit_top = topTable(fit_ebayes, coef=13, number = nrow(edata), p.value=13, 
                       sort.by='none')
    return(fit_top)
}
limma_list = lapply(edata_list, function(data_sublist){
    lapply(data_sublist, function(data) runLimma(data, design))
})

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

## ----------------- vs clinical data ----------------------

covar_data = clinical_data[,-which(colnames(clinical_data)%in%c("Gender Code", "Gender" ,"Calorie Level","Gender-Specific HDL-C Clinical Cut-Off","HDL TNF-a Responder/Non-Responder"))]
clinical_corr = lapply(edata_list, function(data_sublist){
    lapply(data_sublist, function(data) 
        runCorrTestsforAllVariable(data, covar_data[,-(1:4)]))
}) 

## ----------------- vs dietary data ----------------------
covar_data = diet_data[,-(1:8)] / diet_data$`Cals (kcal)`
diet_corr = lapply(edata_list, function(data_sublist){
    lapply(data_sublist, function(data)
        runCorrTestsforAllVariable(data, covar_data))
})
diet_data = covar_data


## -------------------- unsatuation ------------------------
edata_unsat = sapply(edata_conc,function(col)
    col/fdata$mol_wt) %>% 
    as.data.frame %>%
    mutate(Feature = rownames(edata_conc)) %>%
    mutate(lipid_class = fdata$class) %>%
    filter(lipid_class != "Cholesterol") %>%
    mutate(num_carbon = str_split(
            str_extract(Feature,"\\d\\d\\:\\d"),
            "\\:", simplify = T, n=2)[,1] %>%
            as.integer,
        num_double_bond =str_split(
            str_extract(Feature,"\\d\\d\\:\\d"),
            "\\:", simplify = T, n=2)[,2] %>% 
            as.integer)%>%
    melt(id.var = c("Feature","num_carbon","num_double_bond","lipid_class"),
         variable.name = "Sample_ID",
         value.name = "abundance") %>%
    mutate(
        double_bond = abundance * num_double_bond,
        carbon = abundance * num_carbon)

edata_unsat_global = edata_unsat %>%
    ddply(.(Sample_ID), summarise,
          Unsatuation = (sum(double_bond))/(sum(carbon))) %>%
    mutate(Feature = "Global") %>%
    dcast(Feature ~ Sample_ID, value.var = "Unsatuation")

edata_unsat_class = edata_unsat %>%
    ddply(.(Sample_ID, lipid_class), summarise,
          Unsatuation = (sum(double_bond))/(sum(carbon))) %>%
    dcast(lipid_class~Sample_ID, value.var = "Unsatuation") %>%
    setnames(old = "lipid_class", new = "Feature") %>%
    filter(Feature != "Cholesterol")

edata_unsat = rbind(edata_unsat_global, edata_unsat_class) %>%
    column_to_rownames("Feature")

## -------------------- Statistics ------------------------
limma_unsat = runLimma(edata_unsat, design)

covar_data = clinical_data[,-which(colnames(clinical_data)%in%c("Gender Code", "Gender" ,"Calorie Level","Gender-Specific HDL-C Clinical Cut-Off","HDL TNF-a Responder/Non-Responder"))]
unsat_clinical_corr = runCorrTestsforAllVariable(edata_unsat, covar_data[,-(1:4)])

unsat_diet_corr = runCorrTestsforAllVariable(edata_unsat, diet_data)

Unsatuation = list(
    edata = edata_unsat,
    limma = limma_unsat,
    clinical_corr = unsat_clinical_corr,
    diet_corr = unsat_diet_corr
)

## -------------------- chain length ------------------------
edata_sum = sapply(edata_conc,function(col)
    col/fdata$mol_wt) %>% 
    as.data.frame %>%
    mutate(Feature = rownames(edata_conc)) %>%
    mutate(class = fdata$class) %>%
    filter(class != "Cholesterol") %>%
    mutate(chain_length = as.integer(
               str_replace(
                   str_extract(Feature, "\\d\\d\\:\\d"),
                   "\\:\\d",""))) %>%
    mutate(num_fa = ifelse(
        class %in% c("LPC", "CE"), 1,
        ifelse(
            class == "TG", 3,
            ifelse(
                class == "Cholesterol", 0, 2
            )
        )
    )) %>%
    melt(id.var=c("Feature","class", "chain_length", "num_fa"),
         variable.name = "Sample_ID",
         value.name = "abundance") 

edata_len_global = edata_sum %>%
    ddply(.(Sample_ID), summarise,
          chain_length = (sum(abundance * chain_length / num_fa)) / (sum(abundance)) ) %>%
    mutate(Feature = "Global") %>%
    dcast(Feature~Sample_ID , value.var = "chain_length")

edata_len_class = edata_sum %>%
    ddply(.(Sample_ID, class), summarise,
          chain_length = (sum(abundance * chain_length / num_fa)) / (sum(abundance)) ) %>%
    setnames(old = "class", new = "Feature") %>%
    dcast(Feature~Sample_ID, value.var = "chain_length")

edata_len = rbind(edata_len_global, edata_len_class) %>%
    column_to_rownames("Feature")

## -------------------- Statistics ------------------------
limma_len = runLimma(edata_len, design)

len_clinical_corr = runCorrTestsforAllVariable(edata_len, covar_data[,-(1:4)])
len_diet_corr = runCorrTestsforAllVariable(edata_len, diet_data)

Chain_Length = list(
    edata = edata_len,
    limma = limma_len,
    clinical_corr = len_clinical_corr,
    diet_corr = len_diet_corr
)

## ------------------------ save -------------------------------
save(edata_list, fdata, pdata,limma_list, clinical_data, 
     clinical_corr, diet_data, diet_corr, Unsatuation, Chain_Length,
     file="Rdata/lpd_precalc.Rdata")
