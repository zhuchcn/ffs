clado_FF = function(){
    
    mcb = data$data$mcb$proportion
    otu = mcb$otu
    otu = Metabase::subset_samples(otu, otu$sample_table$Treatment == "FF")
    design = model.matrix(
        data = as(otu$sample_table, "data.frame"),
        ~ Timepoint + Subject + 1
    )
    otu = Metabase::as_phyloseq(otu)
    otu = phylox::fix_duplicate_tax(otu)
    spy = phylox::summarizeFromPhyloseq(otu)
    
    spy_stats = phylox::spy_to_limma(spy, design, coef = 2, p.value = 2)
    
    tr = microbiomeViz::parsePhyloseq(otu,use_abundance = F, 
                                      node.size.scale = 1.25)
    p = microbiomeViz::tree.backbone(tr, size = 1)
    anno.data = phylox::create_annodata(spy_stats, coef = "pvalue", 
                                        colors = pal_lancet()(2))
    microbiomeViz::clade.anno(p, anno.data) + 
        geom_point(
        data=data.frame(x=1:2,  y=1:2, color=c("Increase","Decrease")),
        aes(x=x, y=y, color=color), size=0, stroke=0) +
        guides(
            color = guide_legend(
                override.aes = list(size=3, color= rev(c("#00468BFF", "#ED0000FF")))
            )) +
        theme(
            legend.text = element_text(size = 11),
            plot.margin = margin(l=0, r=160, t=0, b=0)
        )
}

clado_Med = function(){
    
    mcb = data$data$mcb$proportion
    otu = mcb$otu
    otu = Metabase::subset_samples(otu, otu$sample_table$Treatment == "Med")
    design = model.matrix(
        data = as(otu$sample_table, "data.frame"),
        ~ Timepoint + Subject + 1
    )
    otu = Metabase::as_phyloseq(otu)
    otu = phylox::fix_duplicate_tax(otu)
    spy = phylox::summarizeFromPhyloseq(otu)
    
    spy_stats = phylox::spy_to_limma(spy, design, coef = 2, p.value = 2)
    
    tr = microbiomeViz::parsePhyloseq(otu,use_abundance = F, 
                                      node.size.scale = 1.25)
    p = microbiomeViz::tree.backbone(tr, size = 1)
    anno.data = phylox::create_annodata(spy_stats, coef = "pvalue", 
                                        colors = pal_lancet()(2))
    microbiomeViz::clade.anno(p, anno.data) +
        geom_point(
            data=data.frame(x=1:2,  y=1:2, color=c("Increase","Decrease")),
            aes(x=x, y=y, color=color), size=0, stroke=0) +
        guides(
            color = guide_legend(
                override.aes = list(size=3, color= rev(c("#00468BFF", "#ED0000FF")))
            )) +
        theme(
            legend.text = element_text(size = 11),
            plot.margin = margin(l=0, r=160, t=0, b=0)
        )
}

clado_mix = function(){
    
    mcb = data$data$mcb$proportion
    otu = mcb$otu
    otu$sample_table$Treatment = factor(otu$sample_table$Treatment,
                                        levels = c("FF", "Med"))
    otu$sample_table$Timepoint = factor(otu$sample_table$Timepoint,
                                        levels = c("Pre", "Post"))
    otu$sample_table$Subject = factor(otu$sample_table$Subject)
    design = model.matrix(
        data = as(otu$sample_table, "data.frame"),
        ~ Timepoint*Treatment + Subject + 1
    )
    otu = otu %>%
        Metabase::as_phyloseq() %>%
        phylox::fix_duplicate_tax()
    
    spy = phylox::summarizeFromPhyloseq(otu)
    spy_stats = phylox::spy_to_limma(spy, design, coef = 13, p.value = 13)
    
    tr = microbiomeViz::parsePhyloseq(otu,use_abundance = F, 
                                      node.size.scale = 1.25)
    p = microbiomeViz::tree.backbone(tr, size = 1)
    anno.data = phylox::create_annodata(spy_stats, coef = "pvalue", 
                                        colors = pal_lancet()(2))
    anno.data = anno.data[-27,]
    microbiomeViz::clade.anno(p, anno.data) +
        geom_point(
            data=data.frame(x=1:2,  y=1:2, color=c("FF","Med")),
            aes(x=x, y=y, color=color), size=0, stroke=0) +
        guides(
            color = guide_legend(
                override.aes = list(size=3, color= rev(c("#00468BFF", "#ED0000FF")))
            )) +
        theme(
            legend.text = element_text(size = 11),
            plot.margin = margin(l=0, r=160, t=0, b=0)
        )
    
}
