library(shiny)
library(tidyverse)

# 2. ricodifica delle modalità di una variabile qualitativa
cat_to_cat <- function(x, cats_in, cats_out){
  if(is.factor(x) | is.character(x)){
    recode_list <- map(cats_in, ~.)
    names(recode_list) <- cats_out
    out <- forcats::fct_recode(x, !!!recode_list)
    return(out)
  }else{
    print("This is an error message")
  }
}


cat_to_cat_UI <- function(id){
  fillRow(flex = c(1, 1), height = "100%",
          fillCol(width = "400px", height = "100%",
                  div(
                    uiOutput(NS(id, "ui_rec"))
                  )
          ),
          fillCol(width = "400px", height = "100%",
                  DT::dataTableOutput(NS(id, "tab_check"), height = "40em")
          )
  )
}

cat_to_cat_SERVER <- function(id, x){
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
    
    output$ui_rec <- renderUI({
      if(!(is.factor(x())) & !(is.character(x()))){
        "The input variable is not a factor or a character variable"
      } else{
      lvls <- levels(x())
      n_lvls <- length(lvls)
      div(
      textInput(NS(id, "new_name"), "New variable name", value = "new_name"),
      tagList(
        purrr::map(1:n_lvls,
                   ~textInput(inputId = NS(id, paste0("label_", .x)),
                              label = paste("Label for category ", lvls[.x]),
                              value = " ")
        ))
      )}
    })
    
    labs_cat_to_cat <- reactive({
      req(input$label_1)
      lvls <- levels(x())
      n_lvls <- length(lvls)
      out <- numeric(n_lvls)
      for(k in 1:n_lvls){
        out[k] <- input[[paste0("label_", k)]]
      }
      out
    })
    
    
    x_ric <- reactive({
      cat_to_cat(x(), levels(x()), labs_cat_to_cat())
    })
    
    output$tab_check <-     DT::renderDT({
      req(input$label_1)
      req(input$new_name)
      data <- data.frame(
        orig_var = x(),
        b = x_ric()
      )
      names(data)[2] <- input$new_name
      DT::datatable(data, rownames = FALSE, fillContainer = TRUE, options = list(
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "700px",
        fixedColumns = FALSE)) 
    })
    
    reactive({
      out <- list(a = x_ric())
      names(out) <- input$new_name
      out
    })
    
  })
}

# ui <- fluidPage(
#   uiOutput("id_ui_test")
#   )
# 
# 
# server <- function(input, output, session) {
#   x <- reactive({factor(sample(letters[1:5], 50, replace = TRUE))  })
#   
#   output$id_ui_test <- renderUI({
#     cat_to_cat_UI("id_cat")
#   })
#   
#   mod <- cat_to_cat_SERVER("id_cat", x)
# }
# 
# shinyApp(ui, server) 
# 
# 
