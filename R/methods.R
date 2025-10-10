methodsUI <- function(id) {
  ns <- NS(id)

  tagList(
    h2(HTML("<strong>Methods</strong>")),
    hr(style = "border-top: 3px solid #044d48;"),

    bslib::navset_pill(
      bslib::nav_panel(title = "Summary of Methods",
                br(),
                imageOutput(ns("methods1_figure"))
      ),
      bslib::nav_panel(title = "Methods Flow Chart",
                br(),
                imageOutput(ns("methods2_figure"))
      )
    )
  )

}

methodsServer <- function(input, output, session, sectionHeader) {
  
  # -------------------------------------------- UI Elements ------------------------------------
  output$section_header <- renderUI({
    sectionHeader()
  })
  
  output$methods1_figure <- renderImage({
    
    # Specify the file path of the image
    img_path <- system.file("extdata", "methods1.png", package = "eligmodel")
    
    list(src = img_path, contentType = 'image/png')
    
  }, deleteFile = FALSE)
  
  output$methods2_figure <- renderImage({
    
    # Specify the file path of the image
    img_path <- system.file("extdata", "methods2.png", package = "eligmodel")

    list(src = img_path, contentType = 'image/png')
    
  }, deleteFile = FALSE)
  
}
