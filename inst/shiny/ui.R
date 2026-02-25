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
    .popover {
        max-width: 800px !important;
         
        --bs-popover-header-bg: #69AED5;  
        --bs-popover-header-color: white;
    }
    .scroll-box {
        height: 300px;       /* Fixed height */
        overflow-y: auto;    /* Enable vertical scroll */
        border: 1px solid #ccc;
        padding: 10px;
        background-color: #f9f9f9;
    }
  ")),
  tags$script(HTML("
    // Function to hide all Bootstrap popovers
   function hideAllPopovers() {
      var popovers = document.querySelectorAll('[data-bs-toggle=\"popover\"]');
      popovers.forEach(function(el) {
        var popoverInstance = bootstrap.Popover.getInstance(el);
        if (popoverInstance) {
          popoverInstance.hide();
        }
      });
    }

    // Close popovers when clicking anywhere outside them
    document.addEventListener('click', function(e) {
      // If click is NOT inside a popover or its trigger
      if (!e.target.closest('.popover') && !e.target.closest('[data-bs-toggle=\"popover\"]')) {
        hideAllPopovers();
      }
    });

    // Close popovers on any input change (text, select, checkbox, etc.)
    document.addEventListener('input', function(e) {
      hideAllPopovers();
    });

    // Close popovers on Escape key
    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape') {
        hideAllPopovers();
      }
    });
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
        9,
        navset_tab(
          nav_panel("Profile", uiOutput("profile")),
          nav_panel("Timeline", 
                    uiOutput("demographics"),
                    plotlyOutput("timeline", height = "500px"))
        )
      ),
      column(
        3,
        h3("Database"),
        textOutput("database"),
        h3("Phenotype"),
        textOutput("phenotype"),
        conditionalPanel(
          condition = "output.adjudicator != ''",
          h3("Reviewer"),
          textOutput("adjudicator"),
        ),
        h3("Person ID"),
        textOutput("personId"),
        wellPanel(
          h3("Decision",
             popover(
               icon("circle-info", style="font-size: 17px; color: #336b92"),
               instructions,
               title = "Decision"
             )),
          radioButtons("decision",
                       "Decision",
                       choices = c("Case", "Non case", "Insufficient information"),
                       selected = character(0)),
          numericInput("indexDay", "Correct index day", value = NULL, step = 1),
        ),
        h3("Patient selection"),
        tags$table(tags$tr(
          tags$td(actionButton("previousButton", "<")),
          tags$td(div(textInput("profileIndex", "", value = "1", width = 50, updateOn = "blur"), style = "margin-top: -10px"), align = "right"),
          tags$td(textOutput("maxLabel")),
          tags$td(actionButton("nextButton", ">"), align = "right"),
        ), width = "100%"),
        h3("Color legend"),
        tags$ul(
          tags$li(div("Disease of interest", style = "color: #11A08A")),
          tags$li(div("Both", style = "color: #000000")),
          tags$li(div("Alternative diagnoses", style = "color: #EB6622")),
          tags$li(div("Other", style = "color: #999999"))
        ),
      )),
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "cosmo",
    )
  ))
