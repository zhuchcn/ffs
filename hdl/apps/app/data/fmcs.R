setwd("~/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/analysis/hdl/apps/app/data")
library(webchem); library(fmcsR)
library(ChemmineR); library(ChemmineOB)

load("data.rda")
mset = data$data$lpd$feature$Proportion

inchikey = mset$feature_data$InChIKey
inchikey = gsub("\\?$", "", inchikey)
inchikey = gsub("\\-$", "-N", inchikey)
inchikey = str_split_fixed(inchikey, " or ", n = 2)[,1]

token = "jH7XGzyXgVHVHP3J5F1Iiaxy3Zw8HrL"

inchi = cts_convert(inchikey, from = "inchikey", to = "inchi code")
smiles = cs_convert(inchi, from = "inchi", to = "smiles", token = token)
smiles[114] = "[H][C@@](COC([*])=O)(COP([O-])(=O)OCC[N+](C)(C)C)OC([*])=O"
smiles[120] = "CCCCCCCCCCCCCCC=COCC(COP(=O)(O)OCCN)OC(=O)CCCCCCCCCC=CCC=CCCCCC"
smiles[121] = scan(what = "character", allowEscapes = FALSE)
# then input "CCCCCCCCCCCCCC/C=C\OC[C@H](COP(=O)(O)OCCN)OC(=O)CC/C=C\C/C=C\C/C=C\C/C=C\C/C=C\C/C=C\CC"
smiles[132] = "*[C@@H](O)[C@@H](NC(=O)*)COP(OCC[N+](C)(C)C)(=O)[O-]"
smiles[133] = "C[N+](C)(C)CCOP([O-])(=O)OC[C@H](NC([*])=O)[C@H](O)[*]"
smiles = as.character(smiles)
names(smiles) = featureNames(mset)

sdfSet = smiles2sdf(smiles)

save(sdfSet, file = "sdfSet.rda")

sdfsubset = sdfSet[1:10]

mcs_li = lapply(cid(sdfsubset), function(x) fmcsBatch(sdfsubset[x], sdfsubset, numParallel = 3))

tanimoto.mat = sapply(mcs_li, function(x) x[, "Tanimoto_Coefficient"])
overlap.mat  = sapply(mcs_li, function(x) x[, "Overlap_Coefficient"])

save(mcs_li, tanimoto.mat, overlap.mat, file = "similar_matrix.rda")

hc = hclust(dist(tanimoto.mat), method="complete")
tree = as.phylo(hc)

class.dat = data.frame(
    class = mset$feature_data$class[mset$feature_data$Annotation == tree$tip.label], 
    stringsAsFactors = F
)
rownames(class.dat) = tree$tip.label

change.dat = t(mset$conc_table) %>% as.data.frame %>% 
    mutate(Treatment = mset$sample_table$Treatment,
           Timepoint = mset$sample_table$Timepoint,
           Subject   = mset$sample_table$Subject) %>%
    melt(id.var = c("Treatment", "Timepoint", "Subject")) %>%
    dcast(Subject + Treatment + variable ~ Timepoint) %>%
    mutate(change = (Post - Pre)/(Post + Pre) * 2) %>%
    dcast(Subject + variable ~ Treatment, value.var = "change") %>%
    mutate(change = Med - FF) %>%
    group_by(variable) %>%
    summarize(Med = mean(Med), FF = mean(FF)) %>%
    #summarize(change = mean(change)) %>%
    arrange(variable == tree$tip.label) %>%
    as.data.frame %>%
    #mutate(change = scale(change)) %>%
    mutate( Med = scale(Med), FF = scale(FF) ) %>%
    column_to_rownames("variable")

anno = data.frame(
    class = class.dat$class,
    #change = change.dat$change,
    FF = change.dat$FF,
    Med = change.dat$Med,
    label = tree$tip.label
) 
min = min(min(anno$FF), min(anno$Med))
max = max(max(anno$FF), max(anno$Med))

anno = anno %>%
    mutate(FF = cut(FF, breaks = seq(min, max, length.out = 100)),
           Med = cut(Med, breaks = seq(min, max, length.out = 100))) %>%
    column_to_rownames("label")

colors = c(
    pal_npg()(9), 
    colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)
)
names(colors) = c(levels(anno$class), levels(anno$FF))

anno.t = rownames_to_column(anno, "label") %>%
    melt(id.vars = "label")

p = ggtree(tree)

gheatmap(p, anno, offset = 0, width = 0.3) +
    scale_fill_manual(values = colors[anno.t$value], guide = F)+
    #theme(legend.position =  "none") +
    coord_polar(theta = "y") +
    geom_point(data = class.dat, 
               aes(x=0, y=0, fill = class), size = 0, stroke = 0) +
    scale_fill_manual("class", 
                     values = pal_npg()(9),
                     guide = guide_legend(
                         override.aes = list(
                             color = pal_npg()(9)
                         )
                     ))

lgd1 = change.dat %>%
    rownames_to_column("id") %>%
    melt(id.vars = "id") %>%
    ggplot() +
    geom_tile(aes(x = variable, y = id, fill = value)) +
    scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, "RdBu")))(100) ) 
lgd1 = get_legend(lgd1)
lgd2 = class.dat %>%
    rownames_to_column("id") %>%
    ggplot() +
    geom_tile(aes(x=1, y = id, fill = class)) +
    scale_fill_npg()
lgd2 = get_legend(lgd2)

grid.arrange(p2, lgd1, lgd2, layout_matrix = rbind(c(1,2),c(1,3)),
             widths = c(5,1))
