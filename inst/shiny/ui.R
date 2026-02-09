instructions <- tagList(
  p("Please determine whether the patient had the phenotype."),
  p("Remember that recording a diagnosis for a disease could occur either because the patient had the disease or as justification for performing a diagnostic procedure to determine whether the patient has the disease. 
A diagnosis by itself or accompanied with only diagnostic procedures may therefore be insufficient evidence, even if recorded more than once. 
Lack of additional evidence of the phentoype other than the diagnosis and diagnostic procedures probably means that the patient was only being tested and does not actually have the phentoype. 
However, it is unlikely that a patient will be tested many times over, so an abundance of diagnoses usually implies the patient has the disease."),
  h4("Decision Threshold"),
  p("Clinical data is rarely 100% definitive. 
You must use your medical judgment to determine the most probable clinical reality based on the available evidence. 
Do not require absolute certainty to make a determination."),
  h4("Final Determination"),
  p("In your final summary, provide one of the following conclusions:"),
  tags$ul(
    tags$li(tags$b("Yes"), ": The preponderance of evidence suggests the patient likely had the phentoype (e.g., specific treatments, consistent symptoms, or repeated diagnoses over time)."),
    tags$li(tags$b("No"), ": The preponderance of evidence suggests the patient likely did not have the phentoype (e.g., it is more likely they were only being tested, the diagnosis was ruled out, or the codes were purely administrative)."),
    tags$li(tags$b("Insufficient information"), ": The data is too ambiguous or vague that it is impossible to estimate which scenario is more likely. Do not use this label simply because the data are imperfect; if one scenario is even slightly more plausible than the other, choose", tags$b("Yes"), "or", tags$b("No"),"."),
  ),
  p("If your final determination is", tags$b("Yes"), ", also provide the relative day that is the likely day of onset. If there is no clear day of onset, simply enter 0.")
)


shinyUI(
  fluidPage(
    tags$style(HTML("
    .tooltip-inner {
      max-width: 500px;
      white-space: normal;
      text-align: left;
    }
  ")),
    fluidRow(
      div(
        tags$table(tags$tr(
          tags$td(titlePanel("Knowledge-Enhanced Electronic Profile Review (KEEPER)", windowTitle = "KEEPER")), 
          tags$td(img(src = "Logo.png", height = "75px", width = "59px", align = "right"))
        ),
        width = "100%"),
        style = "background-color: #336b92; color: white")
      ),
    fluidRow(
      style = "margin-top: 20px",
      column(
        10,
        uiOutput("profile")
      ),
      column(
        2,
        h3("Database"),
        textOutput("database"),
        h3("Phenotype"),
        textOutput("phenotype"),
        textOutput("user"),
        h3("Person ID"),
        textOutput("personId"),
        h3("Color legend"),
        tags$ul(
          tags$li(div("Disease of interest", style = "color: #1F425A")),
          tags$li(div("Alternative diagnoses", style = "color: #EB6622")),
          tags$li(div("Other", style = "color: #5C9EC3"))
        ),
        wellPanel(
          h3("Adjudication",
             tooltip(
               icon("circle-info", style="font-size: 17px; color: #336b92"),
               placement = "left",
               instructions
             )),
          radioButtons("decision",
                       "Decision",
                       choices = c("Case", "Non case", "Insufficient information"),
                       selected = character(0)),
          numericInput("indexDay", "Correct index day", value = NULL, step = 1),
        ),
        h3("Patient selection"),
        actionButton("previousButton", "<"),
        actionButton("nextButton", ">")
      )),
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cosmo",
    )
  ))
