shinyUI(
  fluidPage(
    fluidRow(
      div(titlePanel("Knowledge-Enhanced Electronic Profile Review (KEEPER)", windowTitle = "KEEPER"), style = "background-color: #336b92; color: white"),
      column(
        10,
        uiOutput("profile")
      ),
      column(
        2,
        tags$label(class = "control-label", `for` = "personId", "Person ID"),
        textOutput("personId"),
        actionButton("previousButton", "<"),
        actionButton("nextButton", ">"),
        div(
        radioButtons("decision",
                     "Decision",
                     choices = c("Case", "Non case", "Insufficient information"),
                     selected = character(0)),
        numericInput("indexDay", "Correct index day", value = NULL, step = 1),
        style = "border: 1px solid #DDDDDD; padding: 5px; margin-top: 4px; margin-bottom: 4px",
        
        )
        ,
        h3("Color legend"),
        tags$ul(
          tags$li(div("Disease of interest", style = "color: black")),
          tags$li(div("Alternative diagnoses", style = "color: red")),
          tags$li(div("Other", style = "color: gray"))
        )
      )),
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cosmo",
    )
  ))

# library(bslib)
# ui <- page_sidebar(
#   
#   title = "Knowledge-Enhanced Electronic Profile Review (KEEPER)",
#   
#   sidebar = sidebar(
#     tags$label(class = "control-label", `for` = "personId", "Person ID"),
#     textOutput("personId"),
#     actionButton("previousButton", "<"),
#     actionButton("nextButton", ">"),
#     radioButtons("decision",
#                  "Decision", 
#                  choices = c("Case", "Non case", "Insufficient information"),
#                  selected = character(0)),
#     numericInput("indexDay", "Correct index day", value = NULL, step = 1)
#   ),
#   
#   uiOutput("profile")
#   # theme = bs_theme(version = 5, bootswatch = "flatly")
# )

