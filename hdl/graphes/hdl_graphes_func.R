## ---------------------- load packages -------------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble', 'plotly', 'DT',
         "data.table",'limma','ggthemes','ggplot2','ggthemr', 'ggsci')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## ---------------------- Boxplot for Nutri ---------------------
ggboxplot_for_nutri = function(
    data,
    variables,
    pdata = pdata,
    Subj = "Subj",
    TX = "TX",
    Day = "Day"
){
    df = data[,variables]  %>%
        mutate(
            TX = pdata[,TX],
            Day = pdata[,Day],
            Subj = pdata[,Subj]
        ) %>%
        melt(id.var = (c("Subj","TX","Day")),
             variable.name = "Nutr",
             value.name = "Value") %>%
        filter(Day=="Post")
    
    y.upper.limit = diff(range(df$Value)) * 0.05 + max(df$Value)
    y.lower.limit = min(df$Value) - diff(range(df$Value)) * 0.05
    
    p = ggplot(df, aes(TX,Value)) +
        stat_boxplot(geom = "errorbar", width = 0.5) +
        geom_boxplot(aes(fill = TX)) +
        geom_hline(yintercept = y.upper.limit)+
        scale_y_continuous(labels = function(x){paste(x, "-")},    
                           sec.axis = dup_axis(breaks = 0),
                           expand = c(0, 0)) +
        facet_wrap(~Nutr, nrow=1, strip.position = "bottom") +
        coord_cartesian(ylim = c(y.lower.limit, y.upper.limit)) +
        scale_fill_npg() +
        theme_bw() +
        labs(x = "",
             y = "") +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            #axis.title.y = element_text(size = rel(1.25)),
            axis.title.y.right = element_blank(),
            axis.text.y.right = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(margin = margin(r=0)),
            strip.text = element_text(size = rel(1.25)),
            strip.background = element_blank(),
            panel.spacing = unit(0, "mm")
        )
    return(p)
}

## ----------------------- Boxplot ------------------------------
ggboxplot = function(
    data,
    variable.name,
    samples_on_columns = TRUE,
    pdata = pdata,
    Subj = "Subj",
    TX = "TX",
    Day = "Day"
){
    if(!samples_on_columns){
        data = as.data.frame(t(data))
    }
    
    df = data.frame(
        Var = as.numeric(data[variable.name,]),
        TX = pdata[,TX],
        Day = pdata[,Day],
        Subj = pdata[,Subj]
    )
    p = ggplot(df, aes(x = Day, y = Var)) +
        geom_boxplot() +
        geom_point(aes(colour = Subj)) +
        geom_line(aes(group = Subj, colour = Subj)) +
        facet_grid(.~TX)+
        scale_color_npg() +
        theme_bw() +
        theme(
            legend.position = "none"
        )
    return(p)
}