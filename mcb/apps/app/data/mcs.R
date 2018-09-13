library(ChemmineR); library(fmcsR)
load("../Rdata/bga_precalc.Rdata")

smiles_ids <- fdata$smiles_id
names(smiles_ids) <- fdata$Metabolite_Name
sdfSet = smiles2sdf(smiles_ids[!is.na(smiles_ids)])
cid(sdfSet) = makeUnique(cid(sdfSet))

mcs_mat = sapply(cid(sdfSet), function(x) fmcsBatch(sdfSet[x], sdfSet, au=0, bu=0)[,"Overlap_Coefficient"])

save(mcs_mat, file="mcs.Rdata")