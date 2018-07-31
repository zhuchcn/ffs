## ---------------------- load packages -------------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble', 'plotly', 'DT',
         "data.table",'limma','ggthemes','ggplot2','ggthemr', 'ggsci')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

## ------------------------ diet boxplot -----------------------
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/hdl_structure_and_function")
load("Rdata/lpd_precalc.Rdata")
source("graphes/hdl_graphes_func.R")

## ---------------------- Boxplot for Nutri ---------------------
ggboxplot_for_nutri = function(
    data,
    variables,
    metadata = pdata,
    Subj.name = "Subj",
    TX.name = "TX",
    Day.name = "Day",
    labels = NULL
){
    df = data[,variables] %>%
        mutate(
            TX = metadata[,TX.name],
            Day = metadata[,Day.name],
            Subj = metadata[,Subj.name]
        ) %>% 
        melt(id.var = (c("Subj","TX","Day")),
             variable.name = "Nutr",
             value.name = "Value") %>%
        filter(Day=="Post")
    
    if(!is.null(labels)){
        df = mutate(df,
                    Nutr = factor(Nutr,
                                  levels = variables,
                                  labels = labels))
    }
    
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
             y = "Nutrient (gram / kcal)") +
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

## ------------------------ make the plot ----------------------
diet_boxplot = ggboxplot_for_nutri(
    data = diet_data, 
    variables = c("SatFat (g)", "MonoFat (g)","PolyFat (g)", "Fib (g)"),
    labels = c("SatFat", "MonoFat", "PolyFat", "Fib"))
save(diet_boxplot, file = "graphes/plots/diet_boxplot.Rdata")