output$bga.tree = renderPlot({
    load("data/mcs.rda")
    hc <- hclust(dist(mcs_mat), method="complete")
    tree = as.phylo(hc)
    p = ggtree(tree) +
        geom_tiplab(size=4) +
        scale_x_continuous(limits=c(0,5)) +
        theme_tree2() +
        #geom_text2(aes(subset=!isTip, label=node), hjust=-0.3) +
        # geom_text(aes(subset=isTip, x=x, y=y, label=label), hjust=0) +
        geom_hilight(node=94, fill="steelblue")+
        geom_cladelabel(node=94, label="Choline Metabolites", offset=2) +
        geom_hilight(node=95, fill="darkgreen")+
        geom_cladelabel(node=95, label="Acyl Carnitines", offset=2) +
        geom_hilight(node=104, fill="steelblue")+  
        geom_cladelabel(node=104, label="Bile Acids", offset=2) +
        geom_hilight(node=143, fill="darkgreen") +    # indoles
        geom_cladelabel(node=143, label="Indole Metabolites", offset=2) +
        theme(
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = margin(0,2,0,2)
        )
    p
}, height = 880)