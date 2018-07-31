## -------------------------------------------------------------
pkgs = c("dplyr", "stringr", "reshape2", "tibble", "data.table", "readxl", 
         "Metabase", "webchem")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/data")
##%######################################################%##
#                                                          #
####                      Lipidome                      ####
#                                                          #
##%######################################################%##

file = "../raw_data/lipidome/mx 302870_Zhu_CSH-QTOF MS lipidomics_03-2017_submit.xlsx"

Lipidome = import_wcmc_excel(
    file = file,
    sheet = "Submit",
    conc_range = "I8:BA611",
    feature_range = "A7:H611",
    sample_range = "H1:BA7",
    InChIKey = "InChI Key",
    experiment_type = "Lipidomics"
)
# -------- remove unannotated features -----------------------------------------
Lipidome = subset_features(Lipidome, !is.na(feature_data(Lipidome)$InChIKey))
# -------- seummarize qc -------------------------------------------------------
Lipidome = collapse_QC(Lipidome, qc_names = paste0("Biorec00", 1:5))
# -------- filter out or fill up NAs -------------------------------------------
Lipidome = subset_features(
    Lipidome, apply(conc_table(Lipidome), 1, function(x) sum(is.na(x)) < 5) )
Lipidome = transform_by_feature(
    Lipidome, function(x) ifelse(is.na(x), min(x, na.rm = TRUE)/2, x)
)
# -------- calibration to internal standards -----------------------------------
standards = read.csv("../raw_data/lipidome/wcmc_lipidomics_standards.csv")
feature_data(Lipidome)$class = assign_lipid_class(feature_data(Lipidome)$Annotation)
feature_data(Lipidome)$ESI = ifelse(grepl("\\+$", feature_data(Lipidome)$Species),
                                    "pos", "neg")
experiment_data(Lipidome)$institute = "West Coast Metabolomics Center"
experiment_data(Lipidome)$sample_volumn_ul = 20
experiment_data(Lipidome)$internal_standards = standards
Lipidome = calibrate_lipidomics_wcmc(Lipidome, cid = "InChIKey", 
                                     class = "class", ESI = "ESI")
# -------- if detected in both modes, keep the one with lower cv ---------------
Lipidome = filter_by_cv(Lipidome, cv = "qc_cv", cid = "InChIKey")
## -------- formate annotation -------------------------------------------------
Lipidome$feature_data$Annotation = lipid_name_formater(Lipidome$feature_data$Annotation)
sampleNames(Lipidome) = gsub("\\-", "", sampleNames(Lipidome))
## -------- get molwt ----------------------------------------------------------
inchikey = Lipidome$feature_data$InChIKey 
inchikey = gsub("\\?$", "", inchikey)
inchikey = str_split_fixed(inchikey, " or ", n = 2)[,1]
inchikey = ifelse(grepl("-$",inchikey), paste0(inchikey, "N"), inchikey)

mw_cts = cts_compinfo(inchikey)
mw_cts = sapply(mw_cts, function(x) if(is.na(x)) NA else x$molweight )

mw_cts["TWXNOZDNXVHOLP-HBZIWDQGNA-N"] = 809.593445
mw_cts["XEBSIEVNFRMOOU-XGDVQHKZNA-N"] = 770.114
mw_cts["QICWUUGBNFIZAZ-VEOGPPBONA-N"] = 792.1195
mw_cts["YXSZOBWVQJIWNE-LUJNSPTCSA-N"] = 772.646
mw_cts["FJJANLYCZUNFSE-QVMKKYBKSA-N"] = 786.661
Lipidome$feature_data$molwt = mw_cts
# make annotation as it's feature names
Lipidome$feature_data$Annotation = make.unique(Lipidome$feature_data$Annotation, sep = " ")
featureNames(Lipidome) = Lipidome$feature_data$Annotation
## -------------------------------------------------------------
## --------- D I E T  &  C L I N I C A L   D A T A -------------
## -------------------------------------------------------------
#
# unit for total protein: mg/ml 
# unit for ApoA1: mg/dl
#
file = "../raw_data/diet_data/FFS All Data 2-16-18.xlsx"
diet_data = cbind(
    read_xlsx(file, sheet=1, range="A1:F41"),
    read_xlsx(file, sheet=1, range="VD1:AAH41")) %>%
    as.data.frame
diet_data = diet_data[,-c(3,5)]
diet_data = diet_data[, 1:32]
diet_data[, 'Time'] = gsub("0","",diet_data[, 'Time'])
rownames(diet_data) = str_c('FFS',diet_data[,"Subject ID"], diet_data[,'Timepoint'])
colnames(diet_data)[c(2,4)] = colnames(diet_data)[c(4,2)]
colnames(diet_data)[1] = "Subject"
diet_data$Subject = factor(diet_data$Subject)
diet_data$Treatment = factor(diet_data$Treatment)
diet_data$Timepoint = factor(diet_data$Timepoint, levels = c("Pre", "Post"))

Diet = MultiSet(
    conc_table = conc_table(t(diet_data[,-(1:4)])),
    sample_table = sample_table(diet_data[,1:4]),
    experiment_data = MultiExperimentData(experiment_type = "Dietary Nutrient")
)

clinical_data = cbind(
    read_xlsx(file, sheet=1, range="G1:W41"),
    read_xlsx(file, sheet=1, range="AA1:AX41")
) %>% as.data.frame
clinical_data = clinical_data[,-c(4,11,12,16,21:23,25:27,29:31,33:35)]
clinical_data = tidyr::fill(clinical_data, "Calorie Level", .direction = "down")
clinical_data = cbind(diet_data[,1:4], clinical_data)
clinical_data = mutate(clinical_data,
       `SAA (per ug total protein)` = `SAA (ng/mL)`/`HDL Total Protein`,
       `HDL ApoA1 (per ug total protein)` = `HDL ApoA1`/`HDL Total Protein`)
rownames(clinical_data) = rownames(diet_data)
hdl_function = clinical_data[, c(1:6, 17:20, 30:31)]
HDL_Function = MultiSet(
    conc_table = conc_table(t(hdl_function[,5:12])),
    sample_table = sample_table(hdl_function[,1:4]),
    experiment_data = MultiExperimentData(experiment_type = "HDL Functions and Proteins")
)
clinical_data = clinical_data[, c(1:4, 7:9, 12:15, 21:29)]
Clinical = MultiSet(
    conc_table = conc_table(t(clinical_data[, -(1:6)])),
    sample_table = sample_table(clinical_data[,1:6]),
    experiment_data = MultiExperimentData(experiment_type = "Clinical Values")
)

Lipidome$sample_table$Treatment = Clinical$sample_table$Treatment
Lipidome$sample_table$Timepoint = Clinical$sample_table$Timepoint
Lipidome$sample_table$Subject = Clinical$sample_table$Subject

## -------------------------------------------------------------
## --------------- G L Y C O P E P T I D E S -------------------
## -------------------------------------------------------------

## ---------------------- load data ----------------------------
file='../raw_data/glycopeptides/FFS AD data normalized.xlsx'

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
pdata = diet_data[,c(1,3,4)]
edata = edata[,rownames(pdata)]

# construct the data list
Glycopeptide = list(
    peptide = MultiSet(
        conc_table = conc_table(as.matrix(edata[is.na(fdata$`N/O glycan`),])),
        sample_table = sample_table(pdata),
        feature_data = feature_data(fdata[is.na(fdata$`N/O glycan`),]),
        experiment_data = MultiExperimentData(experiment_type = "Peptides")
    ),
    glycans = GlycomicsSet(
        conc_table = conc_table(as.matrix(edata[!is.na(fdata$`N/O glycan`),])),
        sample_table = sample_table(pdata),
        feature_data = feature_data(fdata[!is.na(fdata$`N/O glycan`),])
    )
)

## ---------------------------------------------------------
## ---------------- S A V E   R D A T A --------------------
## ---------------------------------------------------------
save(Lipidome, Glycopeptide, HDL_Function, Clinical, Diet, 
     file = 'hdl.Rdata')

