## -------------------------------------------------------------
library(reshape2);library(plyr);library(dplyr);library(stringr);library(data.table)
library(Biostrings);library(tidyr);library(tibble);library(readxl)
rm(list=ls())
## -------------------------------------------------------------
## ----------------- L I P I D O M I C S -----------------------
## -------------------------------------------------------------

## ----------------------- load data ---------------------------
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/raw_data/lipidome")

edata_lpd = read_xlsx(
    path = 'mx 302870_Zhu_CSH-QTOF MS lipidomics_03-2017_submit.xlsx',
    sheet = 'Submit',
    col_names = as.character(
        read_xlsx('mx 302870_Zhu_CSH-QTOF MS lipidomics_03-2017_submit.xlsx',
                  sheet = 'Submit',
                  col_names = F,
                  range = 'I1:BA1')
    ),
    range = "I8:BA611") %>% as.data.frame
fdata_lpd = read_xlsx(
    path = 'mx 302870_Zhu_CSH-QTOF MS lipidomics_03-2017_submit.xlsx',
    sheet = 'Submit',
    range = 'A7:H611'
) %>% as.data.frame
pdata_lpd = read.csv('metadata.csv', stringsAsFactors = F)

## -------------------------------------------------------------
pdata_lpd$Label = gsub('\\-','',pdata_lpd$Label)
pdata_lpd$Day = ifelse(pdata_lpd$Day == '0', 'Pre', 'Post')
pdata_lpd$TX[pdata_lpd$TX == 'HD'] = 'Med'
pdata_lpd = column_to_rownames(pdata_lpd, 'Label')
pdata_lpd$Subj = factor(pdata_lpd$Subj)
pdata_lpd$TX = factor(pdata_lpd$TX)
pdata_lpd$Day = factor(pdata_lpd$Day, levels=c('Pre','Post'))

rownames(fdata_lpd) = str_c(
    'LPD', str_pad(rownames(fdata_lpd), width = 3, pad='0'))
names(fdata_lpd) = gsub(' ','',names(fdata_lpd))
names(fdata_lpd) = sub('\\/', '', names(fdata_lpd))
fdata_lpd$Annotation = gsub(' ','',fdata_lpd$Annotation)
fdata_lpd$istd = grepl('iSTD', fdata_lpd$Annotation)
fdata_lpd$ESI = ifelse(grepl('\\+$', fdata_lpd$Species),
                       'positive', 'negative')

names(edata_lpd) = gsub('\\-','',names(edata_lpd))
rownames(edata_lpd) = rownames(fdata_lpd)

## keep only annotated features
fdata_lpd = fdata_lpd[!is.na(fdata_lpd$Annotation),]
edata_lpd = edata_lpd[rownames(fdata_lpd),]

## separate qc samples
edata_qc = edata_lpd[,1:5]
edata_lpd = edata_lpd[,-(1:5)]

## ----------------- Assign Lipid Class ------------------------
get_class = function(xx){
    get_a_class = function(x){
        classes = c('CE','Cholesterol','CUDA','LPC','LPE','PC','PE','PG','SM',
                    'Sphingosine','Ceramide','DG','MG','MAG','TG','FA',
                    'GlcCer', 'ceramide')
        for(class in classes){
            if(grepl(class, x)){
                if(class == 'MAG')  return('MG')
                if(class == 'GlcCer') return('Ceramide')
                if(class == 'ceramide') return('Ceramide')
                else  return(class)
                }
        }
    return(NA)
    }
    return(sapply(xx, get_a_class))
}
fdata_lpd$class = get_class(fdata_lpd$Annotation)
fdata_lpd$class[12] = 'DG2'
## --------------- Calculate Concentration ---------------------
# Unit of concentration: ug/ml
#### Clean the standards table.
standards = read.csv('standards.csv', stringsAsFactors = F)
standards$class = get_class(standards$standard)
standards$class[1] = 'CE'
standards$class[8] = 'FA'
standards$class[4] = 'LPC'
standards$class[15] = 'LPE'
standards$class[13] = 'DG2' # This will only use DG(12:0/12:0) as istd
standards$spiked_vol = ifelse(standards$class=='CE', 750, 225)
standards$spiked_amt = standards$stock * standards$spike / 82.5 
standards$spiked_amt[1]=standards$stock[1]*standards$spike[1]/275

for(i in 1:ncol(edata_lpd)){
    for(j in 25:nrow(fdata_lpd)){
        x = edata_lpd[j,i]
        class = fdata_lpd$class[j]
        istd = fdata_lpd$class[j]
        ESI = fdata_lpd$ESI[j]
        xistd = edata_lpd[fdata_lpd$istd & 
                              fdata_lpd$class == istd &
                              fdata_lpd$ESI == ESI,i] 
        spiked_amt = standards$spiked_amt[standards$class==class]
        spiked_vol = standards$spiked_vol[standards$class==class]
        
        y = x/xistd * spiked_amt * spiked_vol/20
        
        edata_lpd[j,i] = y
    }
}

## -------------- use qc to filter features --------------------
edata_qc = edata_qc[rownames(fdata_lpd),]
fdata_lpd$qc_mean = apply(edata_qc,1,mean) 
fdata_lpd$qc_sd = apply(edata_qc,1,sd)
fdata_lpd$qc_cv = fdata_lpd$qc_sd / fdata_lpd$qc_mean

# qcs
for(i in which(grepl("mean", names(fdata_lpd)) | grepl("sd", names(fdata_lpd)) )){
    for(j in which(!grepl("iSTD$",fdata_lpd$Annotation))){
        x = fdata_lpd[j,i]
        class = fdata_lpd$class[j]
        istd = fdata_lpd$class[j]
        ESI = fdata_lpd$ESI[j]
        xistd = fdata_lpd[fdata_lpd$istd & 
                              fdata_lpd$class == istd &
                              fdata_lpd$ESI == ESI,i] 
        spiked_amt = standards$spiked_amt[standards$class==class]
        spiked_vol = standards$spiked_vol[standards$class==class]
        
        y = x/xistd * spiked_amt * spiked_vol/20
        
        fdata_lpd[j,i] = y
    }
}

fdata_lpd = fdata_lpd[!fdata_lpd$istd,]
edata_lpd = edata_lpd[rownames(fdata_lpd),]

filterWithCv = function(fdata){
    fdata_clean = NULL
    for(i in 1:nrow(fdata)){
        Annot = fdata$Annotation[i]
        if(nrow(fdata[fdata$Annotation==Annot,]) == 1){
            fdata_clean = rbind(fdata_clean, fdata[i,])
        } else if(Annot %in% fdata_clean$Annotation) next
            else{
            fdata_i = fdata[fdata$Annotation == Annot,]
            fdata_keep = fdata_i[order(fdata_i$qc_cv,decreasing=T)[1],]
            fdata_clean = rbind(fdata_clean, fdata_keep)
        }
    }
    return(fdata_clean)
}
fdata_lpd = filterWithCv(fdata_lpd)
edata_lpd = edata_lpd[rownames(fdata_lpd),]
edata_qc = edata_qc[rownames(fdata_lpd),]
## ----------------------- remove NAs --------------------------
#### feature LPD194 has 12 NAs. It is dropped
edata_lpd = edata_lpd[rownames(edata_lpd) != 'LPD194',]
fdata_lpd = fdata_lpd[rownames(fdata_lpd) != 'LPD194',]
edata_qc = edata_qc[rownames(edata_qc) != 'LPD194',]

#### features with 2 NAs are filled up by lowest value devid by 2

fillUpNas = function(data){
    fillUpNa = function(row){
        if(sum(is.na(row)) == 0) return(row)
        else{
            row[which(is.na(row))] = min(row, na.rm=T)/2
            return(row)
        }
    }
    data = apply(data,2,fillUpNa)
    return(as.data.frame(data))
}
edata_lpd = fillUpNas(edata_lpd)

fdata_lpd = cbind(fdata_lpd, edata_qc)
rm(edata_qc)

## -------------------- clean annotation -----------------------
fdata_lpd$Annotation[81] = "Lactosylceramide(d18:1/16:0)"
fdata_lpd$Annotation = ifelse(
    grepl("or", fdata_lpd$Annotation),
    ifelse(
        grepl("A$", fdata_lpd$Annotation),
        str_c(
            str_split(fdata_lpd$Annotation, "or", n=2, simplify = T)[,1],
            "A", sep=""
        ),
        ifelse(
            grepl("B$", fdata_lpd$Annotation),
            str_c(
                str_split(fdata_lpd$Annotation, "or", n=2, simplify = T)[,1],
                "B", sep=""
            ),
            str_split(fdata_lpd$Annotation, "or", n=2, simplify = T)[,1]
        )
    ), fdata_lpd$Annotation
)

for(i in 2:nrow(fdata_lpd)){
    if(fdata_lpd$Annotation[i] %in% fdata_lpd$Annotation[1:(i-1)]){
        fdata_lpd$Annotation[i] = str_c(fdata_lpd$Annotation[i], "B")
    }
}

## ------------------ add molecular weight ---------------------
mw_data = read.csv(
    "Moltecular Weight and Structure.csv",
    header = T, na.strings = "", stringsAsFactors = F) %>%
    mutate(Annotation = ifelse(
        Annotation == "Gal-Gal-Cer(d18:1/16:0)",
        "Lactosylceramide(d18:1/16:0)", 
        Annotation)) %>%
    mutate(Annotation = ifelse(
        Annotation == "PC(p-40:6)",
        "PC(p-40:6)B",
        Annotation
    )) %>%
    select(c("Annotation", "InChI_Key", "MW")) %>% 
    column_to_rownames("Annotation")
mw_data = mw_data[fdata_lpd$Annotation,]
fdata_lpd$mol_wt = mw_data$MW

## ---------------- construct the data list --------------------
lipidome = list(
    edata = edata_lpd,
    fdata = fdata_lpd,
    pdata = pdata_lpd
)


## -------------------------------------------------------------
## --------- D I E T  &  C L I N I C A L   D A T A -------------
## -------------------------------------------------------------
#
# unit for total protein: mg/ml 
# unit for ApoA1: mg/dl
#
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/raw_data/diet_data")
diet_data = cbind(
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="A1:F41"),
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="VB1:AAH41")) %>%
    as.data.frame
diet_data = diet_data[,-c(3,5)]
diet_data[, 'Time'] = gsub("0","",diet_data[, 'Time'])
rownames(diet_data) = str_c('FFS',diet_data[,"Subject ID"], diet_data[,'Timepoint'])

clinical_data = cbind(
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="G1:W41"),
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="AA1:AX41")
) %>% as.data.frame
clinical_data = clinical_data[,-c(4,11,12,16,21,22,23,25,26,27,29,30,31,33,34,35)]
clinical_data = tidyr::fill(clinical_data, "Calorie Level", .direction = "down")
clinical_data = cbind(diet_data[,1:4], clinical_data)
rownames(clinical_data) = rownames(diet_data)
clinical_data = mutate(clinical_data,
       `SAA (per ug total protein)` = `SAA (ng/mL)`/`HDL Total Protein`,
       `HDL ApoA1 (per ug total protein)` = `HDL ApoA1`/`HDL Total Protein`)

## -------------------------------------------------------------
## --------------- G L Y C O P E P T I D E S -------------------
## -------------------------------------------------------------

## ---------------------- load data ----------------------------
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/raw_data/glycopeptides")
file='FFS AD data normalized.xlsx'

# Read in the fdata
fdata = read_excel(path = file, sheet = "SNR 10", range = "A1:G104") %>%
    as.data.frame %>%
    setnames(old=c("X__1","N/O glycn"),new=c("rownames", "N/O glycan")) %>%
    mutate(
        Protein = gsub("H2HSG","A2HSG", Protein),
        rownames = str_c(Protein, Site, 
                         str_replace_na(`N/O glycan`, ""), 
                         str_replace_na(`Composition (Hex HexNAc Fuc Sia)`,""),
                         sep="_"))
fdata = fdata %>%
    mutate(
        rownames = ifelse(
            is.na(Comments), rownames,
            ifelse(
                Comments == "Miscleavage", str_c(rownames, "_MC"),
                ifelse(
                    Comments == "A off", str_c(rownames, "_A_off"),
                    rownames
                    )
                )
            )
        ) %>%
    mutate(
        rownames = ifelse(
            Protein == "ApoE",
            rownames,
            str_c(rownames, 
                  str_replace_na(Comments,""), sep = "_")
        ),
        
        rownames = gsub("\\_{1,3}$","",rownames, perl = T),
        rownames = gsub("Miscleavage","Misc",rownames),
        
        # rownames = gsub("\\ Results","", rownames),
        # rownames = gsub("LGPLVEQGR", "ApoE", rownames),
        # rownames = gsub("Apo_","ApoC3_", rownames),
        
        Normalization = gsub("Apo ","ApoC3 ", Normalization),
        Normalization = gsub("LGPLVEQGR", "ApoE", Normalization)
    ) %>%
    column_to_rownames(var = "rownames")

# Read in the edata
edata = read_excel(path = file, sheet = "SNR 10", 
                   range = "H1:AU104", na = "ND") %>%
    as.data.frame %>%
    mutate(rownames = rownames(fdata)) %>%
    column_to_rownames(var = "rownames")

# line up fdata and pdata
colnames(edata) = gsub("\\-","", colnames(edata))
pdata = lipidome$pdata[,c("Subj", "TX", "Day")]
edata = edata[,rownames(pdata)]

# construct the data list
glycopeptide = list(
    peptides = list(
        edata = edata[is.na(fdata$`N/O glycan`),],
        fdata = fdata[is.na(fdata$`N/O glycan`),],
        pdata = pdata
    ),
    glycans = list(
        edata = edata[!is.na(fdata$`N/O glycan`),],
        fdata = fdata[!is.na(fdata$`N/O glycan`),],
        pdata = pdata
    )
)

## ---------------------------------------------------------
## ---------------- S A V E   R D A T A --------------------
## ---------------------------------------------------------

setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/data")

save(lipidome, glycopeptide, diet_data, clinical_data, file = 'hdl_structure_and_function.Rdata')

