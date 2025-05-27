library(shiny)
library(tidyverse)

add_as_is_UI <- function(id, nm){
  fillRow(flex = c(1, 1), height = "100%",
          fillCol(width = "400px", height = "100%",
                  div(
                    textInput(NS(id, "new_name"), "Variable name", value = nm),
                    uiOutput(NS(id, "ui_rec"))
                  )
                  
          ),
          fillCol(width = "400px", height = "100%",
                  DT::dataTableOutput(NS(id, "tab_check"), height = "40em")
          )
  )
}

add_as_is_SERVER <- function(id, x){
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
    
    output$tab_check <- DT::renderDT({
      data <- data.frame(
        x()
      )
      names(data) <- input$new_name
      DT::datatable(data, rownames = FALSE, fillContainer = TRUE, options = list(
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "700px",
        fixedColumns = FALSE)) 
    })
    
    reactive({
      out <- list(a = x())
      names(out) <- input$new_name
      out
    })
    
  })
}