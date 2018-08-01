load("data/similar_matrix.rda")

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

p = gheatmap(p, anno, offset = 0, width = 0.3) +
    scale_fill_manual(values = colors[anno.t$value], guide = F)+
    #theme(legend.position =  "none") +
    coord_polar(theta = "y")

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

output$lpd_clado = renderPlot({
    grid.arrange(p, lgd1, lgd2, layout_matrix = rbind(c(1,2),c(1,3)),
                 widths = c(5,1))
})