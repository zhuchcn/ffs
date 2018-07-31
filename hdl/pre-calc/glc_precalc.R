## -------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble',"matrixStats","limma")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## ---------------------- load data -------------------------
rm(list=ls())
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/hdl_structure_and_function/')
load('../data/hdl_structure_and_function.Rdata')

## ------------------- extra cleaning ----------------------
fdata = glycopeptide$glycans$fdata
edata = glycopeptide$glycans$edata

fdata = fdata %>%
    rownames_to_column("Feature") %>%
    mutate(
        num_na = apply(glycopeptide$glycans$edata, 1, function(x)
            sum(is.na(x)))
        ) %>% 
    filter(num_na <= 5) %>%
    column_to_rownames("Feature")

fdata = cbind(
    rownames_to_column(fdata, var = "Feature"),
    strsplit(fdata$`Composition (Hex HexNAc Fuc Sia)`, "") %>% 
        sapply(function(x) as.integer(as.character(x[1:4]))) %>% 
        t %>% as.data.frame %>% 
        data.table::setnames(
            old = c("V1", "V2", "V3","V4"),
            new = c("Hex","HexNAc","Fuc","Sia"))
) %>%
    column_to_rownames("Feature") 

edata = edata[rownames(fdata),]
edata = apply(edata, 1, function(row){
    if(sum(is.na(row)) == 0){
        return(row)
    }
    min = min(row, na.rm = T)
    row[is.na(row)] = min/3
    return(row)
}) %>% t %>% as.data.frame

glycopeptide$glycans$edata = edata
glycopeptide$glycans$fdata = fdata

## Data Structure:
# 1. peptide
# 2. glycan
# 3. glycan by position
# 4. glycan by peptide ?
# 5. sugar by position ?
# 6. sugar by peptide ?

## ------------------ glycan by position -------------------
fdata = glycopeptide$glycans$fdata
edata = glycopeptide$glycans$edata
pdata = glycopeptide$glycans$pdata

edata_sum = cbind(fdata[,1:4], edata) %>% 
    melt(id.var = c("Protein", "Site", "N/O glycan", 
                    "Composition (Hex HexNAc Fuc Sia)"),
         variable.name = "Sample_ID", value.name = "abundence") %>% 
    ddply(.(Protein, Site, `N/O glycan`, Sample_ID), summarise,
          abundence = sum(abundence)) %>% 
    dcast(Protein + Site + `N/O glycan` ~ Sample_ID,
          value.var = "abundence") %>%
    mutate(rownames = str_c(Protein, Site, `N/O glycan`, sep = "_")) %>%
    column_to_rownames("rownames")

glycopeptide$glycan_by_position = list(
    edata = edata_sum[,-(1:3)],
    fdata = edata_sum[,1:3],
    pdata = pdata
)

## ------------------ glycan by protein ---------------------
edata_sum = edata_sum %>%
    melt(id.var = c("Protein", "Site", "N/O glycan"),
         variable.name = "Sample_ID",
         value.name = "abundance") %>%
    ddply(.(Protein, Sample_ID), summarise,
          abundance = sum(abundance)) %>%
    dcast(Protein ~ Sample_ID, value.var = "abundance") %>%
    column_to_rownames("Protein")
fdata = data.frame(
    Protein = rownames(edata_sum)
)
rownames(fdata) = rownames(edata_sum)

glycopeptide$glycan_by_protein = list(
    edata = edata_sum,
    fdata = fdata,
    pdata = pdata
)

## ------------------ sugar by position ---------------------
fdata = glycopeptide$glycans$fdata
edata = glycopeptide$glycans$edata
pdata = glycopeptide$glycans$pdata

edata_sum = cbind(fdata[,c(1,2,3,8,9,10,11)], edata) %>%
    rownames_to_column("Feature") %>%
    melt(c("Feature", "Protein", "Site", "N/O glycan",
           "Hex", "HexNAc", "Fuc", "Sia"),
         variable.name = "Sample_ID", value.name = "intensity") %>%
    mutate(total_sugar = Hex + HexNAc + Fuc + Sia) %>% 
    melt(c("Feature","Protein", "Site", "N/O glycan", "total_sugar", "Sample_ID", "intensity"),
         variable.name = "Sugar", value.name = "Number") %>% 
    mutate(intensity = intensity * Number / total_sugar) %>% 
    ddply(.(Protein, Site, `N/O glycan`, Sample_ID, Sugar), summarize,
          intensity = sum(intensity)) %>% 
    dcast(Protein + Site + `N/O glycan` + Sugar ~ Sample_ID,
          value.var = "intensity") %>%
    mutate(
        Feature = str_c(Protein, Site, `N/O glycan`, Sugar, sep = "_")
    ) %>%
    column_to_rownames(var = "Feature")

fdata = edata_sum[,1:4]
edata = edata_sum[,-(1:4)]

glycopeptide$sugar_by_position = list(
    edata = edata,
    fdata = fdata,
    pdata = pdata
)

## ------------------ sugar by peptide ---------------------
edata_sum = edata_sum %>%
    melt(id.var = c("Protein", "Site", "N/O glycan","Sugar"),
         variable.name = "Sample_ID",
         value.name = "abundance") %>% 
    ddply(.(Protein, Sugar, Sample_ID), summarise,
          abundance = sum(abundance)) %>%
    dcast(Protein + Sugar ~ Sample_ID,
          value.var = "abundance") %>% 
    mutate(Feature = str_c(Protein, Sugar, sep = "_")) %>%
    column_to_rownames("Feature")

glycopeptide$sugar_by_protein = list(
    edata = edata_sum[,-(1:2)],
    fdata = edata_sum[,1:2],
    pdata = pdata
)

## ------------------ sugar total ---------------------
edata_sum = edata_sum %>%
    melt(id.var = c("Protein","Sugar"),
         variable.name = "sampleid",
         value.name = "value") %>% 
    ddply(.(Sugar, sampleid), summarise, value = sum(value)) %>%
    dcast(Sugar~sampleid, value.var = "value") %>%
    column_to_rownames("Sugar")

glycopeptide$sugar_total = list(
    edata = edata_sum,
    fdata = data.frame(sugar = rownames(edata_sum)),
    pdata=pdata
)

## ---------------- number of sugar by position ---------------------
fdata = glycopeptide$glycans$fdata
edata = glycopeptide$glycans$edata
pdata = glycopeptide$glycans$pdata

edata_sum = cbind(fdata[,c(1,2,3,8,9,10,11)], edata) %>% 
    melt(c("Protein", "Site", "N/O glycan",
           "Hex", "HexNAc", "Fuc", "Sia"),
         variable.name = "Sample_ID", value.name = "intensity") %>% 
    melt(id.var=c("Protein", "Site", "N/O glycan", "Sample_ID", "intensity"),
         variable.name = "Sugar", value.name = "Number") %>% 
    ddply(.(Protein, Site, `N/O glycan`, Sample_ID, Sugar, Number), summarise,
          sum = sum(intensity)) %>% 
    mutate(Feature = str_c(Protein, Site, `N/O glycan`,Sugar,Number, sep="-")) %>%
    dcast(Feature + Protein + Site + `N/O glycan` + Sugar + Number ~ Sample_ID,
          value.var = "sum") %>%
    column_to_rownames("Feature")

fdata = edata_sum[,1:5]
edata = edata_sum[,-(1:5)]
glycopeptide$sugar_number_by_position = list(
    edata = edata,
    fdata = fdata,
    pdata = pdata
)

## ---------------- number of sugar by protein ----------------------
edata_sum = edata_sum %>%
    melt(id.var = c("Protein","Site","N/O glycan", "Sugar", "Number"),
         variable.name = "sample_id", value.name = "intensity") %>%
    ddply(.(Protein, Sugar, Number, sample_id), summarise,
          sum = sum(intensity)) %>% 
    dcast(Protein + Sugar + Number ~ sample_id, 
          value.var = "sum") %>%
    mutate(feature = str_c(Protein, Sugar, Number, sep="-")) %>%
    column_to_rownames("feature")

fdata = edata_sum[,1:3]
edata = edata_sum[,-(1:3)]
glycopeptide$sugar_number_by_protein = list(
    edata=edata,
    fdata=fdata,
    pdata=pdata
)

## ---------------- number of sugar in total ------------------------
edata_sum = edata_sum %>%
    melt(id.var = c("Protein", "Sugar", "Number"),
         variable.name = "sample_id", value.name = "intensity") %>%
    ddply(.(Sugar, Number, sample_id), summarise,
          sum = sum(intensity)) %>%
    dcast(Sugar + Number ~ sample_id, value.var = "sum") %>%
    mutate(feature = str_c(Sugar, Number, sep="-")) %>%
    column_to_rownames("feature")

fdaata = edata_sum[,1:2]
edata = edata_sum[,-(1:2)]
glycopeptide$sugar_number_total = list(
    edata = edata,
    fdata = fdata,
    pdata = pdata
)

## ----------------------- limma ---------------------------
# limma modeling
runLimma = function(data, design){
    design = model.matrix(data=data$pdata, ~TX + Day + TX*Day + Subj + 1)
    fit = lmFit(data$edata, design)
    fit_ebayes = eBayes(fit)
    fit_top = topTable(fit_ebayes, coef=13, number = nrow(data$edata), p.value=13, 
                       sort.by='none')
    return(fit_top)
}
limma_result = lapply(glycopeptide, function(edata) runLimma(edata, design))

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
        print(names(covar_mat)[i])
        return(fit)
    })
    names(out) = colnames(covar_mat)
    return(out)
}

## ----------------- vs clinical data ----------------------

edata_list = lapply(glycopeptide, function(data) return(data$edata))

covar_data = clinical_data[,c(5,6,9,17,18,20)]
clinical_corr = lapply(edata_list, function(data)
        runCorrTestsforAllVariable(data, covar_data))

## ----------------- vs dietary data ----------------------
covar_data = diet_data[,-(1:8)] / diet_data$`Cals (kcal)`
diet_corr = lapply(edata_list, function(data)
        runCorrTestsforAllVariable(data, covar_data))
diet_data = covar_data

## ------------------------ save -------------------------------
save(glycopeptide,limma_result, clinical_data, 
     clinical_corr, diet_data, diet_corr,
     file="Rdata/glc_precalc.Rdata")
