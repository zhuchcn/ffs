output$mcb.dist_method = renderUI({
    if(input$mcb.ord_method %in% c("PCoA", "NMDS", "MDS")){
        dist = c("bray", "unifrac", "wunifrac", "dpcoa", "jsd")
        if (input$mcb.level != "otu"){
            dist = dist[c(1,4,5)]
        } 
        selectInput("mcb.dist_method", "Select a Distance Method",
                    choices = dist, selected = dist[2])
    }
})

output$mcb.pcoa = renderPlotly({
    
    mset = data$data$mcb$proportion[[input$mcb.level]]
    
    dat = mset$conc_table %>%
        t %>% as.data.frame %>%
        mutate(
            Treatment = mset$sample_table$Treatment,
            Timepoint = mset$sample_table$Timepoint,
            Subject = mset$sample_table$Subject
        ) %>%
        melt(id.var = c("Treatment", "Timepoint", "Subject")) %>%
        dcast(variable + Treatment + Subject ~ Timepoint) %>% 
        mutate(value = Post - Pre) %>%
        dcast(variable ~ Subject + Treatment) %>%
        column_to_rownames("variable")
    
    edata = as.matrix(dat)
    pdata = str_split_fixed(colnames(dat), "_", 2) %>% as.data.frame
    colnames(pdata) = c("Subject", "Treatment")
    rownames(pdata) = colnames(dat)
    
    ps = phyloseq(
        otu_table(edata, taxa_are_rows = TRUE),
        sample_data(pdata)
    )
    if (input$mcb.level == "otu"){
        tax_table(ps) = mset$feature_data %>% as("data.frame") %>% 
            as.matrix %>% tax_table
        phy_tree(ps) = data$data$tree
    }
        
    stats = data$diff$mcb$limma[[input$mcb.level]]
    taxa = rownames(stats)[stats[,input$mcb.pcoa.coef] <= input$mcb.pcoa_cutoff]
    taxa = taxa[taxa != "NA"]
    
    ps = prune_taxa(ps, taxa = taxa)
    
    if (input$mcb.ord_method %in% c("PCoA", "NMDS", "MDS")) {
        ord = ordinate(ps, method = input$mcb.ord_method, 
                       distance = input$mcb.dist_method)
    } else {
        ord = ordinate(ps, method = input$mcb.ord_method)
    }

    plot_ordination(ps, ord, color = "Treatment")
    
})
