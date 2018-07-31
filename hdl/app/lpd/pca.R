lpd_pca_data = reactive({
    limma = data$limma$lpd$feature[[input$lpd.norm]]
    features = rownames(limma)[limma[,input$lpd.p] <= input$lpd.cutoff]
    lpdData = data$data$lpd$feature[[input$lpd.norm]]
    lpdData = subset_features(lpdData, features)
    
    dt = as.data.frame(t(lpdData$conc_table))
    
    if(input$lpd.change){
        dt = dt %>%
            mutate(Timepoint = lpdData$sample_table$Timepoint,
                   Treatment = lpdData$sample_table$Treatment,
                   Subject = lpdData$sample_table$Subject) %>%
            melt(id.var = c("Subject", "Treatment", "Timepoint")) %>%
            dcast(Subject + Treatment + variable ~ Timepoint) %>%
            mutate(change = Post - Pre) %>%
            dcast(Subject + Treatment ~ variable, value.var = "change")
        design = dt[,1:2]
        dt = dt[,-(1:2)]
        rownames = paste0(design$Subject, design$Treatment)
    } else {
        design = lpdData$sample_table[,c("Treatment", "Timepoint", "Subject")]
        rownames = rownames(dt)
    }
    
    if(input$lpd.scale == "log2"){
        dt = sign(dt) * log2(abs(dt) + 1)
    }else if(input$lpd.scale == "z-score sample"){
        dt = t(apply(dt, 1, function(row) scale(row)))
    }else if(input$lpd.scale == "z-score feature"){
        dt = apply(dt, 2, function(col) scale(col))
    }else if(input$lpd.scale == "absolute sample"){
        dt = t(apply(dt, 1, function(x) ((x - min(x))/(max(x) - min(x))-0.5)*2))
    }else if(input$lpd.scale == "absolute feature"){
        dt = apply(dt, 2, function(x) ((x - min(x))/(max(x) - min(x))-0.5)*2)
    }else if(input$lpd.scale == "none"){
        dt = dt
    }
    
    colnames(dt) = features
    rownames(dt) = rownames
    
    list(dt = dt, design = design)
})

output$lpd_pca = renderPlotly({
    
    dt = lpd_pca_data()$dt
    design = lpd_pca_data()$design
    
    prComp = prcomp(dt)
    sdev = prComp$sdev / sum(prComp$sdev)
    df = data.frame(
        PC1 = prComp$x[, "PC1"],
        PC2 = prComp$x[, "PC2"],
        PC3 = prComp$x[, "PC3"],
        Treatment = design$Treatment,
        Subject = design$Subject
    )
    if(!input$lpd.change) {
        df$Timepoint = design$Timepoint
        p = ggplot(df, aes(x = PC1, y = PC2, 
                           color = interaction(Timepoint, Treatment))) +
            geom_point(aes(Treatment = Treatment, Timepoint = Timepoint,
                           Subject = Subject))
    } else {
        p = ggplot(df, aes(x=PC1, y = PC2, color = Treatment)) +
            geom_point(aes(Subject = Subject))
    }
    p = p +
        stat_ellipse() +
        labs(x = str_c("PC1 [ ", round(sdev[1] * 100, 1), "% ]" ),
             y = str_c("PC1 [ ", round(sdev[2] * 100, 1), "% ]" )) +
        scale_color_lancet() +
        theme_bw()
    
    ggplotly(p)
})

output$lpd_heatmap = renderPlotly({
    
    dt = lpd_pca_data()$dt
    design = lpd_pca_data()$design
    
    ColSideColors = 
        if(input$lpd.change) design$Treatment
        else interaction(design$Timepoint, design$Treatment)
    
    heatmaply(t(dt), scale = "none", margins = c(40, 130), 
              ColSideColors = ColSideColors,
              colors = rev(brewer.pal(11, "RdBu")))
    
})