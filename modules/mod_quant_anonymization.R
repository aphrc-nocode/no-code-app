# modules/mod_quant_anonymization.R

# IMPORTANT: keep these global so anon_quant_server_logic() is visible
#source(.anon_server_mod, local = FALSE)

# ---- Quantitative anonymization module  ----
mod_quant_anon_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # Precompute namespaced IDs used in JS strings
  ns_right_panel   <- ns("right-panel")
  ns_main_tabs     <- ns("main_tabs")
  ns_remove_ids    <- ns("remove_ids")        # must exist in identifier_selector output
  ns_remove_inline <- ns("remove_ids_inline")
  ns_method        <- ns("method")
  
  # ACE IDs
  ns_r_ace     <- ns("r_code_ace")
  ns_stata_ace <- ns("stata_code_ace")
  ns_py_ace    <- ns("python_code_ace")
  ns_copy_r    <- ns("copy_r")
  ns_copy_st   <- ns("copy_stata")
  ns_copy_py   <- ns("copy_py")
  
  # Single main section
  ns_dashboard <- ns("dashboard")
  
  shiny::div(
    id    = ns("anon_root"),
    class = "anon-root",
    shiny::tagList(
      shinyjs::useShinyjs(),
      
      shiny::tags$head(
        # Font Awesome (icons)
        shiny::tags$link(
          rel  = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
        ),
        
        # Copy helper JS (works with namespaced ACE ids)
        shiny::tags$script(shiny::HTML("
          function copyAce(editorId, btnId) {
            var ed = ace.edit(editorId);
            if(!ed) return;
            var code = ed.getValue();
            var ta   = document.createElement('textarea');
            ta.value = code;
            document.body.appendChild(ta);
            ta.select();
            document.execCommand('copy');
            document.body.removeChild(ta);
            var btn = document.getElementById(btnId);
            if(btn){
              btn.innerText = 'Copied!';
              setTimeout(function(){ btn.innerText = '📋 Copy'; }, 2000);
            }
          }
        "))
      ),
      
      # ==== MAIN DASHBOARD (visible immediately) ====
      shiny::div(
        id    = ns_dashboard,
        class = "anon-dashboard",
        style = "display:block; padding:20px;",
        
        shiny::tabsetPanel(
          id = ns_main_tabs,
          
          # -------------------------- DASHBOARD TAB -------------------------------
          shiny::tabPanel(
            "Dashboard",
            shiny::fluidRow(
              
              # ---------- LEFT PANEL ----------
              shiny::column(
                width = 4, id = ns("left-panel"),
                
                # Step 0 (platform dataset picker + optional file upload fallback)
                shiny::wellPanel(
                  shiny::tags$h4("Step 0: Load Data"),
                  
                  shiny::div(
                    style = "padding:8px; border:1px solid #eee; border-radius:6px;",
                    shiny::tags$strong("Use already uploaded and Selecetd data"),
                    shiny::br(),
                    shiny::uiOutput(ns("platform_dataset_picker")),
                    shiny::actionButton(
                      ns("use_platform_data"),
                      "Load selected dataset",
                      class = "btn btn-success btn-block"
                    ),
                    shiny::tags$small(
                      style = "display:block; margin-top:6px; color:#666;",
                      ""
                    )
                  ),
                  
                  shiny::tags$hr(),
                  shiny::textOutput(ns("n_obs_text"))
                ),
                
                
                # Step 1
                shiny::wellPanel(
                  shiny::div(
                    class = "qid-header",
                    shiny::tags$h4("Step 1: Remove Direct Identifiers"),
                    shiny::tags$button(
                      id      = ns_remove_inline,
                      class   = "icon-button",
                      title   = "Suppress & Remove Identifiers",
                      onclick = sprintf("$('#%s').click();", ns_remove_ids),
                      shiny::tags$i(class = "fa fa-eraser")
                    )
                  ),
                  shiny::uiOutput(ns("identifier_selector"))
                ),
                
                # Step 2
                shiny::wellPanel(
                  shiny::tags$h4("Step 2: Select Quasi-Identifiers"),
                  shiny::uiOutput(ns("bucket_ui"))
                ),
                
                # Step 3
                shiny::wellPanel(
                  shiny::tags$h4("Step 3: Choose Method & Parameters"),
                  shiny::selectInput(
                    ns_method,
                    "Anonymization Method:",
                    choices = c(
                      "Masking", "Suppression", "Bucketing",
                      "Pseudonymization", "Tokenization",
                      "K-Anonymity",
                      "Generalization",
                      "Anonymize Coordinates"
                    ),
                    selected = "Masking"
                  ),
                  
                  shiny::uiOutput(ns("extra_input")),
                  
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'Generalization'", ns_method),
                    shiny::div(
                      style = "margin-top:8px; padding:10px; background:#f5f5f5; border-radius:4px;",
                      shiny::tags$strong("Generalization tips:"),
                      shiny::tags$ul(
                        shiny::tags$li("Use drag & drop for categorical variables."),
                        shiny::tags$li(shiny::HTML("Switch to <em>Custom numeric ranges</em> for numeric variables. Non-numeric fields will be hidden in that mode.")),
                        shiny::tags$li("Ranges must not overlap or touch.")
                      )
                    )
                  ),
                  
                  shiny::fluidRow(
                    shiny::column(4, shiny::actionButton(ns("apply"), "Apply", class = "btn btn-primary btn-block")),
                    shiny::column(4, shiny::actionButton(ns("undo"),  "Undo",  class = "btn btn-warning btn-block")),
                    shiny::column(4, shiny::actionButton(ns("reset"), "Reset", class = "btn btn-danger btn-block"))
                  ),
                  
                  # Advisor
                  shiny::wellPanel(
                    shiny::tags$h4("Bin-Size Advisor"),
                    shiny::selectInput(
                      ns("advisor_var"),
                      "Choose numeric variable:",
                      choices   = NULL,
                      selectize = TRUE
                    ),
                    shiny::actionButton(ns("advisor_run"), "Show suggestions", class = "btn btn-info btn-block"),
                    shiny::tags$hr(),
                    shiny::tags$h5("Overall Histogram"),
                    shiny::plotOutput(ns("advisor_dist"), height = "220px"),
                    shiny::tags$h5("Summary Statistics"),
                    shiny::verbatimTextOutput(ns("advisor_summary")),
                    shiny::tags$h5("Bin-width Suggestions"),
                    shiny::tableOutput(ns("advisor_table")),
                    shiny::plotOutput(ns("advisor_plot"), height = "220px")
                  ),
                  
                  # Downloads / Report
                  shiny::wellPanel(
                    shiny::checkboxInput(ns("dark_mode"), "Enable Dark Mode"),
                    shiny::tags$hr(),
                    shiny::tags$h4("Downloads"),
                    shiny::downloadButton(ns("download"),        "Download CSV",                class = "btn-block mb-1"),
                    shiny::downloadButton(ns("download_excel"),  "Download Excel",              class = "btn-block mb-1"),
                    shiny::downloadButton(ns("download_dta"),    "Download Stata",              class = "btn-block mb-1"),
                    shiny::downloadButton(ns("download_report"), "Download Risk Report As pdf", class = "btn-block mb-1"),
                    shiny::actionButton(ns("view_report"),       "View Report",                 class = "btn btn-info btn-block"),
                    
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] == 'Anonymize Coordinates'", ns_method),
                      shiny::div(
                        class = "download-note",
                        shiny::HTML("<strong>Note:</strong> When <em>Anonymize Coordinates</em> is the last applied method, the standard downloads above will export the anonymized coordinates. No separate download is needed.")
                      )
                    ),
                    
                    shiny::tags$hr(),
                    shiny::uiOutput(ns("k_report")),
                    shiny::tags$hr(),
                    shiny::tags$h4("Steps Log"),
                    shiny::verbatimTextOutput(ns("step_log"), placeholder = TRUE)
                  )
                )
              ),
              
              # ---------- RIGHT PANEL ----------
              shiny::column(
                width = 8, id = ns_right_panel,
                shiny::div(
                  class = "right-containers",
                  
                  # === Container 1: Data Preview / Map ===
                  shiny::div(
                    class = "right-box",
                    shiny::div(class = "right-header", shiny::tags$h3("Data Preview")),
                    shiny::div(
                      class = "right-body",
                      
                      shiny::conditionalPanel(
                        condition = sprintf("input['%s'] == 'Anonymize Coordinates'", ns_method),
                        leaflet::leafletOutput(ns("geo_map"), height = "420px")
                      ),
                      
                      shiny::conditionalPanel(
                        condition = sprintf("input['%s'] != 'Anonymize Coordinates'", ns_method),
                        shiny::div(
                          id    = ns("preview-table"),
                          class = "table-wrap",
                          shiny::tableOutput(ns("preview_merged"))
                        )
                      )
                    )
                  ),
                  
                  # === Container 2: Risk Assessment ===
                  shiny::div(
                    class = "right-box",
                    shiny::div(class = "right-header", shiny::tags$h3("Risk Assessment")),
                    shiny::div(
                      class = "right-body",
                      shiny::div(
                        class = "risk-summary-grid",
                        shiny::div(class = "summary-card", shiny::uiOutput(ns("risk_before"))),
                        shiny::div(class = "summary-card", shiny::uiOutput(ns("risk_after")))
                      ),
                      shiny::tags$br(),
                      shiny::div(
                        id = ns("preview-gauges"),
                        shiny::fluidRow(
                          shiny::column(
                            width = 6,
                            shiny::div(
                              class = "gauge-box",
                              shiny::tags$h4("Risk Before"),
                              flexdashboard::gaugeOutput(ns("gauge_before"), height = "200px")
                            )
                          ),
                          shiny::column(
                            width = 6,
                            shiny::div(
                              class = "gauge-box",
                              shiny::tags$h4("Risk After"),
                              flexdashboard::gaugeOutput(ns("gauge_after"), height = "200px")
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # ----------------------------- CODES TAB ---------------------------------
          shiny::tabPanel(
            "Codes",
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::tags$h4("R Code"),
                shinyAce::aceEditor(
                  outputId = ns_r_ace,
                  mode     = "r",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                shiny::actionButton(
                  ns_copy_r, "📋 Copy",
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s')", ns_r_ace, ns_copy_r)
                )
              ),
              shiny::column(
                width = 4,
                shiny::tags$h4("Stata Code"),
                shinyAce::aceEditor(
                  outputId = ns_stata_ace,
                  mode     = "stata",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                shiny::actionButton(
                  ns_copy_st, "📋 Copy",
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s')", ns_stata_ace, ns_copy_st)
                )
              ),
              shiny::column(
                width = 4,
                shiny::tags$h4("Python Code"),
                shinyAce::aceEditor(
                  outputId = ns_py_ace,
                  mode     = "python",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                shiny::actionButton(
                  ns_copy_py, "📋 Copy",
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s')", ns_py_ace, ns_copy_py)
                )
              )
            )
          ),
          
          # -------------------------- DESCRIPTIONS TAB ----------------------------
          shiny::tabPanel(
            "Descriptions",
            shiny::uiOutput(ns("descriptions_panel"))
          )
        )
      )
    )
  )
}

# Allow passing rv_current from main server to anon logic
mod_quant_anon_server <- function(id, rv_current = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (exists("anon_quant_server_logic", mode = "function", inherits = TRUE)) {
      get("anon_quant_server_logic", mode = "function", inherits = TRUE)(
        input, output, session,
        rv_current = rv_current
      )
    } else {
      shiny::showNotification(
        "anon_quant_server_logic() not found. Check server/anon/server_module_quant.R",
        type = "error"
      )
    }
  })
}
