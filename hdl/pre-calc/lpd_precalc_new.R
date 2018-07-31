pkgs = c("dplyr", "reshape2", "tibble", "Metabase", "MatCorR")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/hdl/pre-calc")
## -------- load data ----------------------------------------------------------
load("../../data/hdl.Rdata")
## -------- transform lpd data -------------------------------------------------
lpd_conc = Lipidome
lpd_prop = transform_by_sample(Lipidome, function(x) x/sum(x))
lpd_prt  = transform_by_feature(Lipidome, function(x)
    x/conc_table(HDL_Function)["HDL Total Protein",])

lpd_class_conc = summarize_features(lpd_conc, feature_var = "class")
lpd_class_prop = summarize_features(lpd_prop, feature_var = "class")
lpd_class_prt  = transform_by_feature(lpd_class_conc, function(x) 
    x/conc_table(HDL_Function)["HDL Total Protein",])

molwt = Lipidome$feature_data$molwt
lpd_mol = transform_by_sample(Lipidome, function(x) x/molwt)
lpd_mol$experiment_data$conc_table_unit = "umol/ml"
lpd_eod = summarize_EOD(lpd_mol, name = "Annotation", class = "class")
lpd_acl = summarize_ACL(lpd_mol, name = "Annotation", class = "class")
lpd_odd = summarize_odd_chain(lpd_mol, name = "Annotation", class = "class")
lpd_ratio = summarize_lipid_ratios(lpd_mol, name = "Annotation", class = "class")

Lipidome = list(
    class = list(
        Concentration = lpd_class_conc,
        Proportion    = lpd_class_prop,
        Adj_protein   = lpd_class_prt
    ),
    feature = list(
        Concentration = lpd_conc,
        Proportion    = lpd_prop,
        Adj_protein   = lpd_prt
    ),
    summarize = list(
        EOD18          = lpd_eod,
        ACL            = lpd_acl,
        "Odd Chain"    = lpd_odd,
        "Lipid Ratios" = lpd_ratio
    )
)
## -------- limma --------------------------------------------------------------
design = model.matrix(data=as(sample_table(HDL_Function), "data.frame"), 
                      ~Treatment * Timepoint + Subject + 1)

limma_list = lapply(Lipidome, function(sublist){
    lapply(sublist, function(data) 
        mSet_limma(data, design, coef = 13, p.value = 13))
})
## -------- correlation --------------------------------------------------------
design2 = model.matrix(data=as(sample_table(HDL_Function), "data.frame"), 
                       ~Subject + 1)
corr_func = function(covar){
    message(substitute(covar))
    lapply(Lipidome, function(sublist){
        lapply(sublist, function(lpd){
            message("I'm working...")
            MatCorPack(X = conc_table(covar), Y = conc_table(lpd), 
                       design = design2)
        })
    })
}
corr_fct = corr_func(HDL_Function)
corr_diet = corr_func(Diet)
corr_clinical = corr_func(Clinical)
## -------- save ---------------------------------------------------------------
save(Lipidome, HDL_Function, Clinical, Diet,
     limma_list, corr_fct, corr_diet, corr_clinical,
     file = "../Rdata/lpd_precalc_new.Rdata")
