shinyUI(
  fluidPage(
    fluidRow(
      titlePanel("Knowledge-Enhanced Electronic Profile Review (KEEPER)"),
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
        radioButtons("decision",
                     "Decision", 
                     choices = c("Case", "Non case", "Insufficient information"),
                     selected = character(0)),
        numericInput("indexDay", "Correct index day", value = NULL, step = 1),
        selectInput("theme_selector", "Select a Theme:",
                    choices = c("cosmo", "flatly", "darkly", "minty", "sketchy", "united", "cyborg", "vapor"),
                    selected = "cosmo"),
      )),
    # shinythemes::themeSelector()
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cosmo",
    )
  ))