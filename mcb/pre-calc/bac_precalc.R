pkgs = c('dplyr','stringr','reshape2','tibble', 'limma','Metabase', 'MatCorR')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
## -------- load data ----------------------------------------------------------
setwd(dirname(parent.frame(2)$ofile))
load('../../data/mcb.Rdata')

## -------- summarization ------------------------------------------------------
bac$feature_data$full_name = featureNames(bac)
featureNames(bac) = bac$feature_data$`short key`

edata = bac$conc_table
sum_data = tibble(
    `Total CA` = colSums(edata[c("CA", "TCA", "GCA"),]),
    `Total CDCA` = colSums(edata[c("CDCA", "TCDCA", "GCDCA"),]),
    `Total DCA` = colSums(edata[c("DCA", "TDCA", "GDCA"),]),
    `Total UDCA` = colSums(edata[c("UDCA", "GUDCA"),]),
    `Unconjugated Primary BA` = colSums(edata[c("CA", "CDCA"),]),
    `Unconjugated Secondary BA` = colSums(edata[c("DCA", "LCA", "UDCA"),]),
    `Conjugated Primary BA` = colSums(edata[c("TCA", "TCDCA", "GCA", "GCDCA"),]),
    `Conjugated Secondary BA` = colSums(edata[c("TDCA", "GDCA", "GUDCA","GHDCA"),])
) %>%
    mutate(
        `Total Primary BA` = `Unconjugated Primary BA` + `Conjugated Primary BA`,
        `Total Secondary BA` = `Unconjugated Secondary BA` + `Conjugated Secondary BA`,
        `Total Conjugated BA` = `Conjugated Primary BA` + `Conjugated Secondary BA`,
        `CA/DCA ratio` = edata["CA",] / edata["DCA",],
        `CDCA/(LCA+UDCA) ratio` = edata["CDCA",] / colSums(edata[c("LCA", "UDCA"),]),
        `Total CA/DCA ratio` = `Total CA` / `Total DCA`,
        `Total CDCA/(LCA+UDCA) ratio` = `Total CDCA` / (edata["LCA",] + `Total UDCA`),
        `Conjugated/Unconjugated CA` = colSums(edata[c("TCA", "GCA"),])/edata["CA",],
        `Conjugated/Unconjugated CDCA` = colSums(edata[c("TCDCA", "GCDCA"),])/edata["CDCA",],
        `Conjugated/Unconjugated DCA` = colSums(edata[c("TDCA", "GDCA"),])/edata["DCA",],
        `Conjugated/Unconjugated UDCA` = edata["GUDCA",]/edata["UDCA",],
        `Conjugated/Unconjugated Primary ratio` = `Conjugated Primary BA`/`Unconjugated Primary BA`,
        `Conjugated/Unconjugated Secondary ratio` = `Conjugated Primary BA`/`Unconjugated Secondary BA`,
        `Unconjugated Primary/Secondary ratio` = `Unconjugated Primary BA` / `Unconjugated Secondary BA`,
        `Conjugated Primary/Secondary ratio` = `Conjugated Primary BA` / `Conjugated Secondary BA`
    ) %>%
    mutate(
        `Total Primary/Secondary ratio` = `Total Primary BA` / `Total Secondary BA`
    ) %>% t
colnames(sum_data) = sampleNames(bac)
bac_sum = MetabolomicsSet(
    conc_table = conc_table(sum_data),
    sample_table = sample_table(bac)
)

bac = list(
    raw = bac,
    summarized = bac_sum
)

## -------- limma --------------------------------------------------------------
design = model.matrix(data=as(bac$raw$sample_table, "data.frame"), 
                      ~Treatment * Timepoint + Subject + 1)
limma_list = lapply(bac, function(li) mSet_limma(li, design, coef = 13, p.value = 13))

## -------- corr ---------------------------------------------------------------
design2 = model.matrix(data=as(bac$raw$sample_table, "data.frame"), 
                       ~Subject + 1)

corr_sfa = lapply(bac, function(li) MatCorPack(X = sfa$conc_table, Y = li$conc_table, design = design2))

## -------- save ---------------------------------------------------------------
save(bac, limma_list, corr_sfa, sfa,
     file = "../Rdata/bac_precalc.Rdata")
