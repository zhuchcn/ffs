## -------------------------------------------------------------
pkgs = c("dplyr", "stringr", "reshape2", "tibble", "data.table", "readxl", 
         "tidyr", "Metabase", "ape")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))

################################################################################
##########                    M I C R O B I O M E                     ##########
################################################################################
otu_table = read.table('../raw_data/microbiome/feature_table.tsv', 
                       sep = '\t', header=T, stringsAsFactor=F, row.names = 1)
tax_table = read.table('../raw_data/microbiome/taxonomy.tsv', 
                       sep='\t', header=T, stringsAsFactor=F, row.names = 1)
sample_data = read.table('../raw_data/microbiome/sample_metadata.tsv', 
                         sep='\t', header=T, stringsAsFactors = F, row.names = 1)
rownames(otu_table) = str_c(
    "MCB", str_pad(rownames(otu_table), width = 4, pad = "0")
)
otu_table = otu_table[,rownames(sample_data)]
rownames(tax_table) = str_c(
    "MCB", str_pad(rownames(tax_table), width = 4, pad = "0")
)

otu_table = otu_table %>% as.matrix %>% conc_table()
sample_data$Timepoint = factor(sample_data$Timepoint, 
                               levels = c("Pre", "Post"))
sample_data$Treatment = factor(sample_data$Treatment,
                               levels = c("FF", "Med"))
sample_data$Subject = gsub("^FF", "", sample_data$StudyID)
sample_data = sample_table(sample_data)
tax_table = feature_data(tax_table)
mcb = MicrobiomeSet(otu_table, sample_data, tax_table)

sampleNames(mcb) = str_c("FFS",mcb$sample_table$SubjectID)

tree = read.tree("../raw_data/microbiome/tree.nwk")
tree$tip.label = str_c("MCB", tree$tip.label)

################################################################################
##########             P I C R U S T   F U N C T I O N S              ##########
################################################################################
files = list.files(
    path = "../raw_data/picrust/", 
    pattern = "seqtab_tax_rarified_norm_ko_l[123].tsv", 
    full.names = TRUE
)
names(files) = c("l1", "l2", "l3")
sample_table = mcb$sample_table
pcr = lapply(files, function(l){
    otu_table = read.delim(l, header = T, skip = 1, 
                           comment.char = "", row.names = 1) 
    otu_table = otu_table[, sample_data$Description]
    colnames(otu_table) = rownames(sample_table)
    otu_table = as.matrix(otu_table)
    MicrobiomeSet(conc_table(otu_table), sample_table)
})
names(pcr) = names(files)

################################################################################
##########               B I O G E N I C   A M I N E S                ##########
################################################################################
bga = import_wcmc_excel(
    file = "../raw_data/biogenic_amines/mx 351193 Zhu_human plasma_HILIC-QTOF MSMS_11-2017_submit.xlsx",
    sheet = "Submit",
    conc_range = "I8:BA1490",
    sample_range = "H1:BA7",
    feature_range = "A7:H1490",
    InChIKey = "InChI Key"
)
bga = collapse_QC(bga, qc_names = paste0("Biorec00", 1:5))
bga = subset_features(bga, !grepl("iSTD$", bga$feature_data$`Metabolite name`) & !is.na(bga$feature_data$InChIKey) & !is.na(bga$feature_data$`Metabolite name`))
bga$sample_table$Treatment = mcb$sample_table$Treatment
bga$sample_table$Timepoint = mcb$sample_table$Timepoint
bga$sample_table$Subject = mcb$sample_table$Subject
Metabase::sampleNames(bga) = gsub("-", "", Metabase::sampleNames(bga))
bga$feature_data$`Metabolite name` = make.unique(bga$feature_data$`Metabolite name`)
featureNames(bga) = bga$feature_data$`Metabolite name`

################################################################################
##########       S H O R T   C H A I N   F A T T Y   A C I D S        ##########
################################################################################
path = "../raw_data/short_chain_fatty_acids/mx 352728 Trevor Zhu_human feces_VSCFA_12-2017_submit.xlsx"
edata = read_excel(path = path, sheet = "submit", 
                   range = "A4:F43", col_names = FALSE) %>%
    as.data.frame %>%
    column_to_rownames("X__1")
colnames(edata) = read_excel(path = path, sheet = 1, range = "B1:F1",
                             col_names = F)
rownames(edata) = gsub("\\d{6}_(FFS)-(\\d{3})-([A-D]{1})_\\d{1,2}", 
                       "\\1\\2\\3", rownames(edata))
edata = t(edata)
pdata = data.frame(
    Treatment = mcb$sample_table$Treatment,
    Timepoint = mcb$sample_table$Timepoint,
    Subject = mcb$sample_table$Subject,
    row.names = sampleNames(mcb)
)
sfa = Metabase::MultxSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata),
    experiment_data = MultiExperimentData(experiment_type = "Short Chain Fatty Acids")
)

################################################################################
##########                    B I L E   A C I D S                     ##########
################################################################################
path = "../raw_data/bile_acids/mx 351239 Zhu_bile acids_human plasma_09-2018 submit.xlsx"
conc_range = "T9:BG31"
sample_range = "S2:BG6"
feature_range = "A8:S31"
bac = import_wcmc_excel(
    file = path, 
    sheet = "Final Submit", 
    conc_range = conc_range, 
    sample_range = sample_range, 
    feature_range = feature_range, 
    InChIKey = "InChIKey"
)
sampleNames(bac) = gsub("\\d{1,2}-FFS-(\\d{3})-([A-D]{1})", "FFS\\1\\2", sampleNames(bac))
featureNames(bac) = bac$feature_data$name
bac$sample_table$Subject = mcb$sample_table$Subject
bac$sample_table$Timepoint = mcb$sample_table$Timepoint
bac$sample_table$Treatment = mcb$sample_table$Treatment

## remove features with more than 10 observations smaller than the LOQ
bac = subset_features(
    bac, 
    sapply(1:nfeatures(bac), function(i)
        sum(bac$conc_table[i,] <= bac$feature_data$`LOQ (nM)`[i]) <= 10)
)

################################################################################
##########               D I E T   &   C L I N I C A L                ##########
################################################################################
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
diet_data$Treatment = factor(diet_data$Treatment, levels = c("FF", "Med"))
diet_data$Timepoint = factor(diet_data$Timepoint, levels = c("Pre", "Post"))

diet = Metabase::MultxSet(
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
clinical_data = clinical_data[, c(1:4, 7:9, 12:15, 21:29)]
cli = Metabase::MultxSet(
    conc_table = conc_table(t(clinical_data[, -(1:6)])),
    sample_table = sample_table(clinical_data[,1:6]),
    experiment_data = MultiExperimentData(experiment_type = "Clinical Values")
)

################################################################################
##########                        E X P O R T                         ##########
################################################################################
save(mcb, pcr, bga, sfa, bac, diet, cli, tree,
     file = "mcb.Rdata")