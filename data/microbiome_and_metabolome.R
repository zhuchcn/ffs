## -------------------------------------------------------------
library(reshape2);library(plyr);library(dplyr);library(stringr)
library(Biostrings);library(tidyr);library(tibble);library(readxl)
rm(list=ls())

## -------------------------------------------------------------
## ------------- B I O G E N I C   A M I N E S -----------------
## -------------------------------------------------------------

## ---------------------- load data ----------------------------
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/data/biogenic_amines')
file='raw_data/mx 351193 Zhu_human plasma_HILIC-QTOF MSMS_11-2017_submit.xlsx'
edata_bga = read_xlsx(
    path = file,
    sheet = 'Submit',
    col_names = as.character(
        read_xlsx(
            path = file,
            sheet = 'Submit',
            col_names = F,
            range = 'I1:BA1')),
    range = 'I8:BA1490'
) %>% as.data.frame
fdata_bga = read_xlsx(
    path = file,
    sheet = 'Submit',
    range = 'A7:H1490'
) %>% as.data.frame
pdata_bga = read.csv('raw_data/metadata.csv',stringsAsFactors = F)
## ------------------------ clean up ---------------------------
names(pdata_bga)[2] = "Sample_number"
pdata_bga = pdata_bga[pdata_bga$Sample_number != 'QC',]
pdata_bga = pdata_bga[,1:8]
pdata_bga$Label = gsub('\\-','',pdata_bga$Label)
pdata_bga$Subject = factor(pdata_bga$Subject)
pdata_bga$TX[pdata_bga$TX == 'HD'] = 'Med'
pdata_bga$Day = ifelse(pdata_bga$Day == 0, 'Pre','Post')
pdata_bga$TX = factor(pdata_bga$TX)
pdata_bga$Day = factor(pdata_bga$Day, levels = c('Pre','Post'))
rownames(pdata_bga) = NULL
pdata_bga = column_to_rownames(pdata_bga, var = 'Label')

names(fdata_bga) = gsub(' ', '', names(fdata_bga))
names(fdata_bga) = gsub('\\/', '', names(fdata_bga))
names(fdata_bga)[2] = 'Metabolite_Name'
fdata_bga = fdata_bga[!is.na(fdata_bga$Metabolite_Name),]
fdata_bga$ESI = ifelse(grepl('\\+$', fdata_bga$Species), 
                       'positive', 'negative')

edata_bga = edata_bga[rownames(fdata_bga),]
rownames(fdata_bga) = str_c(
    'BGA', str_pad(rownames(fdata_bga), width=3, pad='0'))
rownames(edata_bga) = rownames(fdata_bga)

fdata_bga = cbind(fdata_bga, edata_bga[,1:5])
edata_bga = edata_bga[,-(1:5)]
names(edata_bga) = gsub('\\-', '', names(edata_bga))
edata_bga = edata_bga[,rownames(pdata_bga)]

for(i in 1:nrow(fdata_bga)){
    if(fdata_bga$Metabolite_Name[i] %in% fdata_bga$Metabolite_Name[1:(i-1)]){
        fdata_bga$Metabolite_Name[i] = str_c(fdata_bga$Metabolite_Name[i]," 2")
    }
}
## ------------------ normalize edata -------------------------
# calculate the TIC
# edata_tic = sapply(edata_bga, sum)
# mTIC = mean(edata_tic)
# edata_bga = t(t(edata_bga) / edata_tic * mTIC) %>%
#     as.data.frame
    
## -------------------  get other ids  -------------------------
load("bga_chem_ids.Rdata")
fdata_bga = fdata_bga %>%
    rownames_to_column("Feature") %>%
    mutate(kegg_id = chem_ids$kegg_id,
           smiles_id = chem_ids$smiles_id) %>%
    column_to_rownames("Feature")

## -------------------------------------------------------------
## ----------------- M I C R O B I O M E -----------------------
## -------------------------------------------------------------
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/data/microbiome')

edata_mcb = read.table('raw_data/feature_table.tsv', sep = '\t', header=T, 
                       stringsAsFactor=F)
fdata_mcb = read.table('raw_data/taxa.tsv', sep='\t', header=T, stringsAsFactor=F)
pdata_mcb = read.table('raw_data/sample_metadata.tsv', sep='\t', header=T, 
                       stringsAsFactors = F)
seqs_mcb = readDNAStringSet('raw_data/rep_seqs.fasta',format='fasta')

## ---------------------------------------------------------
edata_mcb$feature_id = str_c('MCB',
                        str_pad(edata_mcb$feature_id,width=4, pad='0'))
edata_mcb = column_to_rownames(edata_mcb,'feature_id')

fdata_mcb$feature_id = rownames(edata_mcb)
fdata_mcb = column_to_rownames(fdata_mcb, 'feature_id')

pdata_mcb$SampleID = str_c('FFS',pdata_mcb$SubjectID)
pdata_mcb = column_to_rownames(pdata_mcb,'SampleID')
pdata_mcb = pdata_mcb[sort(rownames(pdata_mcb)),]

edata_mcb = edata_mcb[,pdata_mcb$Description]
colnames(edata_mcb) = rownames(pdata_mcb)

pdata_mcb$StudyID = gsub('FF', '', pdata_mcb$StudyID)
pdata_mcb$StudyID = factor(pdata_mcb$StudyID)
pdata_mcb$Treatment = factor(pdata_mcb$Treatment)
pdata_mcb$Timepoint = factor(pdata_mcb$Timepoint, levels = c('Pre','Post'))

names(seqs_mcb) = str_c('MCB', names(seqs_mcb))
seqs_mcb = seqs_mcb[rownames(fdata_mcb)]

## -------------------------------------------------------------
## --------------- Short Chain Fatty Acids ---------------------
## -------------------------------------------------------------
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/data')

scfa_data = read_excel(
    "../raw_data/short_chain_fatty_acids/mx 352728 Trevor Zhu_human feces_VSCFA_12-2017_submit.xlsx",
    sheet = 1, range = "A4:F43", col_names = F) %>%
    as.data.frame %>%
    column_to_rownames("X__1") 
colnames(scfa_data) = read_excel(
    "../raw_data/short_chain_fatty_acids/mx 352728 Trevor Zhu_human feces_VSCFA_12-2017_submit.xlsx",
    sheet = 1, range = "B1:F1", col_names=F
)
rownames(scfa_data) = gsub("171107\\_","", rownames(scfa_data))
rownames(scfa_data) = str_split(rownames(scfa_data),"\\_",n=2, simplify=T)[,1]
rownames(scfa_data) = gsub("\\-","",rownames(scfa_data))

## -------------------------------------------------------------
## ------------------ D I E T   D A T A ------------------------
## -------------------------------------------------------------
setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/raw_data/diet_data/')
diet_data = cbind(
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="A1:F41"),
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="VB1:AAH41")) %>%
    as.data.frame
diet_data = diet_data[,-c(3,5)]
diet_data[, 'Time'] = gsub("0","",diet_data[, 'Time'])
rownames(diet_data) = str_c('FFS',diet_data[,"Subject ID"], diet_data[,'Timepoint'])

clinical_data = cbind(
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="G1:W41"),
    read_xlsx('FFS All Data 2-16-18.xlsx', sheet=1, range="AA1:AV41")
) %>% as.data.frame
clinical_data = clinical_data[,-c(4,11,12,16,19,20,21,23,24,25,27,28,29,31,32,33)]
clinical_data = tidyr::fill(clinical_data, "Calorie Level", .direction = "down")
clinical_data = cbind(diet_data[,1:4], clinical_data)
rownames(clinical_data) = rownames(diet_data)

## ---------------------------------------------------------
## ---------------- S A V E   R D A T A --------------------
## ---------------------------------------------------------

setwd('/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/data')
biogenic_amines = list(
    edata = edata_bga,
    fdata = fdata_bga,
    pdata = pdata_bga
)
microbiome = list(
    edata = edata_mcb,
    fdata = fdata_mcb,
    pdata = pdata_mcb,
    seqs = seqs_mcb
)
save(biogenic_amines, microbiome, scfa_data, diet_data, clinical_data, file = 'microbiome_and_metabolome.Rdata')

# ## ---------------------------------------------------------
# ## ------------------ S A V E   C S V ----------------------
# ## ---------------------------------------------------------
# write.csv(
#     rownames_to_column(edata_lpd, var = 'FeatureID'),
#     'lipidomics/edata_lpd.csv', row.names = F)
# write.csv(
#     rownames_to_column(fdata_lpd, var = 'FeatureID'),
#     'lipidomics/fdata_lpd.csv', row.names = F)
# write.csv(
#     rownames_to_column(pdata_lpd, var = 'SampleID'),
#     'lipidomics/pdata_lpd.csv', row.names = F)
# 
# write.csv(
#     rownames_to_column(edata_bga, var = 'FeatureID'),
#     'biogenic_amines/edata_bga.csv', row.names = F)
# write.csv(
#     rownames_to_column(fdata_bga, var = 'FeatureID'),
#     'biogenic_amines/fdata_bga.csv', row.names = F)
# write.csv(
#     rownames_to_column(pdata_bga, var = 'SampleID'),
#     'biogenic_amines/pdata_bga.csv', row.names = F)
# 
# write.csv(
#     rownames_to_column(edata_mcb, var = 'FeatureID'),
#     'microbiome/edata_mcb.csv', row.names = F)
# write.csv(
#     rownames_to_column(fdata_mcb, var = 'FeatureID'),
#     'microbiome/fdata_mcb.csv', row.names = F)
# write.csv(
#     rownames_to_column(pdata_mcb, var = 'SampleID'),
#     'microbiome/pdata_mcb.csv', row.names = F)
# writeXStringSet(seqs_mcb, 
#                 'microbiome/rep_seqs.fasta', 
#                 format='fasta', width = 10000)
# 
# write.csv(
#     rownames_to_column(diet_data, var = 'SampleID'),
#     'diet_data/diet_data.csv', row.names=F
# )
