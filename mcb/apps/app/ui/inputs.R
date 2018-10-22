methodSelector = function(type, choices){
    selectInput(inputId = paste0(type, ".method"), 
                label = "Select a Correlation Method",
                choices = choices,
                selected = choices[1])
}

output$VarsInput = renderUI({
    type = substr(input$sidebar, 1, 3)
    
    corr_methods = names(data$corr$bga$sfa)
    
    if(type == "mcb") {
        lvls = names(data$data$mcb$count)
        norms = names(data$data$mcb)
        tagList(
            selectInput("mcb.norm", "Count or Proportion?",
                        choices = norms, selected = norms[1]),
            selectInput("mcb.level", "Select the Phylogenic Level:",
                        choices = lvls, selected = lvls[1]),
            methodSelector(type, corr_methods)
        )
    }else if(type == "pcr") {
        tagList(
            selectInput("pcr.level", "Select the Phylogenic Level:",
                        choices = names(data$data$pcr),
                        selected = names(data$data$pcr)[1]),
            methodSelector(type, corr_methods)
        )
    }else if (type == "bac"){
        tagList(
            selectInput("bac.level", "Select the level:",
                        choices = names(data$data$bac),
                        selected = names(data$data$bac)[1])
        )
    }else if(type %in% c("bga")){
        tagList(
            methodSelector(type, corr_methods)
        )
    }
})