instructions <- tagList(
  p("Your task is to determine whether the patient had the phenotype."),
  h3("Diagnostic Context & Rules"),
  p("Remember that a recorded diagnosis code can indicate actual disease presence OR merely serve as justification for a diagnostic procedure (testing to rule out the disease).
  A diagnosis alone, or accompanied only by diagnostic tests, may be insufficient evidence, even if recorded more than once.
  A lack of corroborating evidence (such as specific treatments or consistent symptoms) usually means the patient was only being tested and does not actually have the disease.
  However, it is unlikely that a patient will be tested many times over a long period, so an abundance of diagnoses or diagnoses paired with disease-specific treatments usually implies the patient has the disease."),
  h3("Decision Threshold"),
  p("Clinical data is rarely 100% definitive.
  Use your medical judgment to determine the preponderance of evidence. 
  Do not require absolute certainty; instead, determine the most clinically plausible scenario."),
  h3("Final Determination"),
  p("Carefully consider all the provided information, and paint a mental picture of the most likely scenarios.
  Then, provide one of the following verdicts:"),
  tags$ul(
    tags$li(tags$b("Case"), ": The preponderance of evidence suggests the patient likely had the phentoype (e.g., specific treatments, consistent symptoms, or repeated diagnoses over time)."),
    tags$li(tags$b("Non-case"), ": The preponderance of evidence suggests the patient likely did not have the phentoype (e.g., it is more likely they were only being tested, the diagnosis was ruled out, or the codes were purely administrative)."),
  ),
  h3("Certainty Level"),
  p("Indicate how certain you are in your verdict using one of two levels:"),
  tags$ul(
    tags$li(tags$b("Low"), ": You are closer to a 50-50 chance of being wrong."),
    tags$li(tags$b("High"), ": You are close to certainty of being right. (Note: Do not default to \"low\" for \"Non-case\" verdicts; a pattern of testing without treatment, or explicit alternative diagnosis, warrants a \"high\" certainty \"Non-case\".)"),
  ),
  h3("Onset"),
  p("If your verdict is \"Yes\", provide the relative day that is the likely day of onset. If there is no clear day of onset, return 0.")
)

colorLegendText <- "The color coding is based on a prior classification of concepts. These should be considered as general guidance rather than absolute truth."

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
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
                       choices = c("Case", "Non-case"),
                       selected = character(0)),
          radioButtons("certainty",
                       "Certainty",
                       choices = c("High", "Low"),
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
        h3("Color legend",
           popover(
             icon("circle-info", style="font-size: 17px; color: #336b92"),
             colorLegendText,
             title = "Color legend"
           )),
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
