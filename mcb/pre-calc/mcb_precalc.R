## ------------------- loading librarys ------------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble', 'parallel','DESeq2',
         'Biostrings',"Hmisc",'ALDEx2','limma','data.table')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
rm(list=ls())

## ----------------------- load data -----------------------------
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/mcb/')
load('../data/microbiome_and_metabolome.Rdata')
edata = microbiome$edata; fdata = microbiome$fdata; pdata = microbiome$pdata
seqs = microbiome$seqs

## ----------------- fdata duplicate taxa -----------------------
# Error in data.frame(node = index, row.names = child, isTip, taxaAbun = taxtab$rel_abun) : 
#   duplicate row.names: f__Family_XI, s__finegoldii, s__massiliensis, s__intestinalis, s__vaginalis, s__caccae, s__stercoris, s__bacterium, s__faecis, s__hominis

# Family_XI
feats = which (fdata$Family == 'Family_XI')
fdata$Family[feats] = paste(fdata$Order[feats],fdata$Family[feats],sep="_")

# Now deal with the species
dup_species = c("finegoldii", "massiliensis", "intestinalis", "vaginalis", "caccae", "stercoris", "bacterium", "faecis", "hominis")
for(i in 1:nrow(fdata)){
    if(fdata$Species[i] %in% dup_species){
        fdata$Species[i] = paste(fdata$Genus[i], fdata$Species[i], sep='_')
    }
}

## ------------------ make the edata_list ------------------------
edata_prop = sapply(edata, function(col){
    col/sum(col)
}) %>% as.data.frame() %>%
    mutate(feature = rownames(edata)) %>%
    column_to_rownames("feature")

edata_aldex = aldex.clr(edata, conds = character(length = 40)) %>%
    getMonteCarloInstances() %>% 
    sapply(rowMeans) %>% 
    as.data.frame()

edata_list = list(
    reads = edata,
    proportion = edata_prop,
    clr = edata_aldex
) %>%
    lapply(function(data){
        data_list = lapply(1:8, function(level){
            if(level ==8 ){
                return(data)
            }
            
            sapply(data, function(col){
                tapply(col, fdata[,level], sum)
            })
        })
        names(data_list) = c(colnames(fdata), "Feature")
        return(data_list)
    })

## -------------------- run deseq2 -------------------------
runDESeq2 = function(reads, pdata, resultName = 'TreatmentMed.TimepointPost'){
    
    de = DESeqDataSetFromMatrix(
        reads, pdata,
        ~Treatment + Timepoint + Treatment*Timepoint + StudyID + 1
    )
    fit = DESeq(de, betaPrior = F)
    results = results(fit, name = resultName)
    return(as.data.frame(results))
}

deseq_list = lapply(edata_list$reads, function(reads){
    runDESeq2(reads, pdata)
})

## ------------------ run limma -------------------------
run_limma = function(data, design, coef=13){
    fit = lmFit(data, design)
    fit_ebayes = eBayes(fit)
    fit_result = topTable(fit_ebayes, coef=coef, number = nrow(data), 
                          sort.by="none",p.value = coef)
}

design = model.matrix(
    data=pdata, 
    ~Treatment + Timepoint + Treatment * Timepoint + StudyID + 1
    )

limma_list = lapply(edata_list$proportion, function(data){
    run_limma(data=data, design=design)
})

## -------------------- run aldex.limma ------------------------
aldex.limma = function(reads, design, mc.samples = 128, 
                       limma.par = list(coef=13, p.value=13)){
    
    # make aldex.clr 
    clr = aldex.clr(reads = reads, conds = character(length=40),
                    mc.samples = mc.samples, denom = 'all', useMC=TRUE)
    
    mc.all = getMonteCarloInstances(clr)
    
    # setup limma result containers; will be condensed later
    lm.t.matrix = data.frame(matrix(1, nrow = nrow(reads), ncol = mc.samples))
    lm.logFC.matrix = lm.t.matrix
    lm.AveExpr.matirx = lm.t.matrix
    lm.pvalue.matrix = lm.t.matrix
    lm.padj.matrix = lm.t.matrix
    
    # mc.i is the i-th Monte-Carlo instance
    for(mc.i in 1:mc.samples){
        
        # generate a matrix of i-th Monte-Carlo instance, columns are samples, rows are features
        mc.input = sapply(mc.all, function(x) x[,mc.i])
        
        fit = lmFit(mc.input, design)
        fit_ebayes = eBayes(fit)
        fit_top = topTable(fit_ebayes, coef=limma.par$coef, 
                           number = nrow(mc.input), p.value=limma.par$p.value, 
                           sort.by='none')
        
        lm.logFC.matrix[,mc.i] = fit_top$logFC
        lm.AveExpr.matirx[,mc.i] = fit_top$AveExpr
        lm.t.matrix[,mc.i] = fit_top$t
        lm.pvalue.matrix[,mc.i] = fit_top$P.Value
        lm.padj.matrix[,mc.i] = fit_top$adj.P.Val
    }
    
    lm.result = data.frame(
        logFC = rowMeans(lm.logFC.matrix),
        AveExpr = rowMeans(lm.AveExpr.matirx),
        t = rowMeans(lm.t.matrix),
        P.Value = rowMeans(lm.pvalue.matrix),
        adj.P.Val = rowMeans(lm.padj.matrix),
        row.names = rownames(mc.input)
    )
    return(lm.result)
}
aldex_limma_list = lapply(edata_list$reads, function(reads)
    aldex.limma(reads=reads, design))

## ------------------- make a stats list ------------------------
Differential_Abundance = list(
    deseq2 = deseq_list,
    limma = limma_list,
    aldex.limma = aldex_limma_list
)

### ---------------- correlation function ----------------------
runCorrTests = function(data, covar){
    
    # This is the function for correlation I'm gonna use later    
    my_cor_test = function(x, y, method){
        result = try(cor.test(x,y,method=method), silent = TRUE)
        return(result)
    }
    
    # define the correlation methods
    corr_methods = c("pearson", "spearman", "kendall")
    
    # this will do the calculation for all the methods defined
    corr_list = lapply(corr_methods, function(method){
        apply(data, 1, function(yy){
            y = yy[which(!is.na(covar))]
            x = covar[!is.na(covar)]
            return(my_cor_test(x,y,method=method))
        })
    })
    names(corr_list) = corr_methods
    
    # this will extract the coeficients from result list from previous step
    coef_list = lapply(corr_list, function(method_list){
        alist = lapply(c("estimate", "p.value"), function(coef){
            sapply(method_list, function(covar_list){
                if(class(covar_list) == "try-error") return(NA)
                return(covar_list[[coef]])
            })
        })
        names(alist)=c("estimate", "p.value")
        return(alist)
    })
    
    out.mat = data.frame(
        pearson.r = coef_list$pearson$estimate,
        pearson.pvalue = coef_list$pearson$p.value,
        spearman.rho = coef_list$spearman$estimate,
        spearman.pvalue = coef_list$spearman$p.value,
        kendall.tau = coef_list$kendall$estimate,
        kendall.pvalue = coef_list$kendall$p.value,
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

## ---------------- corr with lm model ------------------------
corrLinearModel = function(data, covar, Subj){
    out_list = lapply(colnames(covar), function(var_name){ # x is a column of the covar matrix
        x = as.numeric(covar[,var_name])
        apply(data, 1, function(y){ # y is a row of the data matrix
            model = summary(lm(y ~ x + Subj + 1))
            result = c(
                model$coefficients[2,1],
                model$coefficients[2,4],
                model$r.squared,
                model$adj.r.squared
            )
            names(result) = c("estimate","pval","r.sq","adj.rsq")
            return(result)
        }) %>% t %>% as.data.frame %>%
            rownames_to_column("Feature") %>%
            mutate(padj = p.adjust(pval, "BH")) %>%
            column_to_rownames("Feature")
    })
    names(out_list) = colnames(covar)
    return(out_list)
}

## --------------- correlation with bga ------------------------
bga_data = biogenic_amines$edata %>%
    mutate(Feature = biogenic_amines$fdata$Metabolite_Name) %>%
    filter(!grepl("iSTD",Feature)) %>%
    remove_rownames() %>%
    column_to_rownames("Feature")

bga_corr = lapply(edata_list, function(data_list){
    lapply(data_list, function(data){
        runCorrTestsforAllVariable(data, as.data.frame(t(bga_data)))
    })
})

bga_corr_lm = lapply(edata_list, function(data_list){
    lapply(data_list, function(data){
        corrLinearModel(data, as.data.frame(t(bga_data)), pdata$StudyID)
    })
})

## --------------- correlation with scfa ----------------------
scfa_corr = lapply(edata_list, function(data_list){
    lapply(data_list, function(data){
        runCorrTestsforAllVariable(data, scfa_data)
    })
})

## --------------- correlation with diet ---------------------
diet_data = diet_data[,c(6,10,11,12,13,14,15,16,27,28,29,30,31,32)] %>%
    sapply(function(col) col/diet_data$`Cals (kcal)`) %>%
    as.data.frame %>%
    mutate(sampleid = rownames(diet_data)) %>%
    column_to_rownames("sampleid")

diet_corr = lapply(edata_list, function(data_list){
    lapply(data_list, function(data){
        runCorrTestsforAllVariable(data, diet_data)
    })
})

## ---------- correlation with clinical values ------------
clinical_data = clinical_data[,c(14,18,19,20,21,22,23,24,25,26,27)]
clinical_corr = lapply(edata_list, function(data_list){
    lapply(data_list, function(data){
        runCorrTestsforAllVariable(data, clinical_data)
    })
})

## --------------------- save -----------------------------

save(edata_list, fdata, pdata, seqs, Differential_Abundance, 
     biogenic_amines, scfa_data, diet_data, clinical_data,
     bga_corr, bga_corr_lm, scfa_corr, diet_corr, clinical_corr,
     file="Rdata/mcb_precalc.Rdata")
