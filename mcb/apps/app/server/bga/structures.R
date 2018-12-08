bgaImgInputGenerator = function(id) {
    selectInput(glue("bga_img_{id}"), "Select Here", choices = featureNames(bga),
                selected = featureNames(bga)[1])
}
bgaImgRenderGenerator = function(id) {
    renderImage({
        
        list(src=glue("img/"), alt = input$feature)
    }, deleteFile = F)
}

bgaStructureOutput = function(id){
    bga = data$data$bga
    tagList(
        box(
            renderUI(glue("bgaImgTitle{id}")),
            renderUI(glue("bgaImgInput{id}")),
            renderUI(glue("bgaImgRender{id}"))
        )
    )
}

output$bga_structures = renderUI({
    tagList(
        bgaStructureOutput("bga_mol_1"),
        bgaStructureOutput("bga_mol_2"),
        bgaStructureOutput("bga_mol_3"),
        bgaStructureOutput("bga_mol_4")
    )
})