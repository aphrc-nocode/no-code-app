# modules/mod_quant_anonymization.R

# ---- Source Anon helper logic (safe) ----
.anon_helper <- normalizePath(file.path("..", "Anon", "anonymization_functions.R"), mustWork = FALSE)
if (file.exists(.anon_helper)) {
  # IMPORTANT: make functions available globally (moduleServer checks by name)
  source(.anon_helper, local = FALSE)
} else {
  message("Anon helper file not found at: ", .anon_helper)
}

# ---- Source Anon module server logic (safe) ----
.anon_server_mod <- normalizePath(file.path("..", "Anon", "server_module_quant.R"), mustWork = FALSE)
if (file.exists(.anon_server_mod)) {
  # IMPORTANT: make anon_quant_server_logic() available globally
  source(.anon_server_mod, local = FALSE)
} else {
  message("Anon module server file not found at: ", .anon_server_mod)
}

# ---- Quantitative anonymization module ----
mod_quant_anon_ui <- function(id) {
  ns <- NS(id)
  
  # Precompute namespaced IDs used in JS strings
  ns_right_panel   <- ns("right-panel")
  ns_main_tabs     <- ns("main_tabs")
  ns_remove_ids    <- ns("remove_ids")            # must exist in identifier_selector output
  ns_remove_inline <- ns("remove_ids_inline")
  ns_method        <- ns("method")
  ns_continue      <- ns("continue")
  ns_view_manual   <- ns("view_manual")
  
  # ACE IDs
  ns_r_ace     <- ns("r_code_ace")
  ns_stata_ace <- ns("stata_code_ace")
  ns_py_ace    <- ns("python_code_ace")
  ns_copy_r    <- ns("copy_r")
  ns_copy_st   <- ns("copy_stata")
  ns_copy_py   <- ns("copy_py")
  
  # Main sections (namespaced)
  ns_landing   <- ns("landing")
  ns_dashboard <- ns("dashboard")
  
  # Safe JS suffix (valid identifier)
  js_suffix <- gsub("[^A-Za-z0-9]", "_", id)
  
  div(
    id    = ns("anon_root"),
    class = "anon-root",
    
    # IMPORTANT: no fluidPage() inside a module. Add only content + head resources here.
    tagList(
      shinyjs::useShinyjs(),
      
      tags$head(
                # Font Awesome
        tags$link(
          rel  = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
        ),
        
        tags$style(HTML("
          .anon-root { position: relative; min-height: 85vh; }

          .anon-root #logo {
            position: absolute;
            top: 20px;
            right: 20px;
            width: 150px;
            height: auto;
            z-index: 20;
          }

          .anon-root .anon-dashboard { min-height: 85vh; }

          .anon-root .anon-landing {
            position: relative;
            width: 100%;
            min-height: 85vh;
            background: url('anon_assets/Image10.png') center / cover no-repeat;
            border-radius: 8px;
            overflow: hidden;
          }

          .anon-root .landing-overlay {
            position: absolute;
            top: 50%; left: 50%;
            transform: translate(-50%, -50%);
            text-align: center;
            color: white;
            width: min(800px, 92%);
            pointer-events: auto;
          }

          .anon-root .landing-overlay .btn { margin: 0 10px; }

          /* Hide numeric text inside gauges */
          .anon-root .gauge .gauge-value, .anon-root .gauge .gauge-label { display: none !important; }

          .anon-root #preview-gauges .gauge-box { display: flex; flex-direction: column; align-items: center; text-align: center; }
          .anon-root #preview-gauges h4 { margin: 0 0 6px 0; }
          .anon-root #gauge_before, .anon-root #gauge_after { margin: 0 auto; }

          /* Horizontal scroll helper for wide tables */
          .anon-root .table-wrap { overflow-x: auto; }

          /* Risk summary alignment */
          .anon-root .risk-summary-grid {
            display: grid; grid-template-columns: 1fr 1fr; gap: 12px; align-items: start;
          }
          .anon-root .summary-card {
            border: 1px solid #e6e6e6; border-radius: 6px; padding: 10px 12px; background: #fafafa;
          }

          /* Copy buttons */
          .anon-root button[id^='copy_'] {
            float: right; font-size: 12px; background-color: #e0e0e0; border: none;
            border-radius: 5px; padding: 5px 10px; box-shadow: 1px 1px 3px rgba(0,0,0,0.2);
            cursor: pointer; margin-bottom: 10px; position: relative; z-index: 10;
          }

          /* Inline suppress icon button */
          .anon-root .qid-header {
            display:flex; align-items:center; justify-content:space-between; gap:8px; margin-bottom:6px;
          }
          .anon-root .icon-button {
            background:#dc3545; color:white; border:none; border-radius:4px;
            padding:0; cursor:pointer; height:32px; width:32px; display:flex; align-items:center; justify-content:center;
          }
          .anon-root .icon-button:hover { opacity:0.9; }
          .anon-root .icon-button .fa { pointer-events:none; }

          /* Map container sizing */
          .anon-root #geo_map { height: 420px; border: 1px solid #e6e6e6; border-radius: 6px; }

          /* Small helper note under downloads */
          .anon-root .download-note {
            font-size: 12px; color: #444; background: #f7f7f7; border: 1px dashed #ddd;
            border-radius: 6px; padding: 8px; margin-top: 8px;
          }
        ")),
        
        
        
        # Copy helper JS (works with namespaced ACE ids)
        tags$script(HTML("
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
        ")),
        
        # Robust "Continue" helper: force-set input value + do a client-side fallback show/hide
        tags$script(HTML(sprintf("
          function anonContinue_%s(){
            try {
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
              }
              var landing = document.getElementById('%s');
              var dash    = document.getElementById('%s');
              if (landing) landing.style.display = 'none';
              if (dash)    dash.style.display    = 'block';
              document.body.classList.add('anon-active');
            } catch(e) {
              if (console && console.log) console.log(e);
            }
          }
        ",
                                 js_suffix,
                                 ns_continue,
                                 ns_landing,
                                 ns_dashboard
        )))
      ),
      
      # ==== LANDING SCREEN (namespaced id) ====
      div(
        id    = ns_landing,
        class = "anon-landing",
        
        tags$img(src = "anon_assets/logo.png", id = "logo"),
        
        div(
          class = "landing-overlay",
          tags$h1("APHRC Data Anonymization Dashboard"),
          
          actionButton(
            ns_view_manual,
            "View Manual",
            class = "btn btn-light btn-lg"
          ),
          
          actionButton(
            ns_continue,
            "Continue",
            class   = "btn btn-primary btn-lg",
            onclick = sprintf("anonContinue_%s();", js_suffix)
          )
        )
      ),
      
      # ==== MAIN DASHBOARD (namespaced id) ====
      div(
        id    = ns_dashboard,
        class = "anon-dashboard",
        style = "display:none; padding:20px;",
        
        tabsetPanel(
          id = ns_main_tabs,
          
          # -------------------------- DASHBOARD TAB -------------------------------
          tabPanel(
            "Dashboard",
            fluidRow(
              # ---------- LEFT PANEL ----------
              column(
                width = 4, id = ns("left-panel"),
                
                # Step 0
                wellPanel(
                  tags$h4("Step 0: Load Data"),
                  fileInput(
                    ns("file"),
                    "Upload CSV, Excel, or Stata File",
                    accept = c(".csv", ".xlsx", ".dta")
                  ),
                  textOutput(ns("n_obs_text"))
                ),
                
                # Step 1
                wellPanel(
                  div(
                    class = "qid-header",
                    tags$h4("Step 1: Remove Direct Identifiers"),
                    tags$button(
                      id    = ns_remove_inline,
                      class = "icon-button",
                      title = "Suppress & Remove Identifiers",
                      onclick = sprintf("$('#%s').click();", ns_remove_ids),
                      tags$i(class = "fa fa-eraser")
                    )
                  ),
                  uiOutput(ns("identifier_selector"))
                ),
                
                # Step 2
                wellPanel(
                  tags$h4("Step 2: Select Quasi-Identifiers"),
                  uiOutput(ns("bucket_ui"))
                ),
                
                # Step 3
                wellPanel(
                  tags$h4("Step 3: Choose Method & Parameters"),
                  selectInput(
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
                  
                  uiOutput(ns("extra_input")),
                  
                  # Guidance blurb (module-safe condition)
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'Generalization'", ns_method),
                    div(
                      style = "margin-top:8px; padding:10px; background:#f5f5f5; border-radius:4px;",
                      tags$strong("Generalization tips:"),
                      tags$ul(
                        tags$li("Use drag & drop for categorical variables."),
                        tags$li(HTML("Switch to <em>Custom numeric ranges</em> for numeric variables. Non-numeric fields will be hidden in that mode.")),
                        tags$li("Ranges must not overlap or touch.")
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(4, actionButton(ns("apply"), "Apply", class = "btn btn-primary btn-block")),
                    column(4, actionButton(ns("undo"),  "Undo",  class = "btn btn-warning btn-block")),
                    column(4, actionButton(ns("reset"), "Reset", class = "btn btn-danger btn-block"))
                  ),
                  
                  # Advisor
                  wellPanel(
                    tags$h4("Bin-Size Advisor"),
                    selectInput(
                      ns("advisor_var"),
                      "Choose numeric variable:",
                      choices   = NULL,
                      selectize = TRUE
                    ),
                    actionButton(ns("advisor_run"), "Show suggestions", class = "btn btn-info btn-block"),
                    tags$hr(),
                    tags$h5("Overall Histogram"),
                    plotOutput(ns("advisor_dist"), height = "220px"),
                    tags$h5("Summary Statistics"),
                    verbatimTextOutput(ns("advisor_summary")),
                    tags$h5("Bin-width Suggestions"),
                    tableOutput(ns("advisor_table")),
                    plotOutput(ns("advisor_plot"), height = "220px")
                  ),
                  
                  # Downloads / Report
                  wellPanel(
                    checkboxInput(ns("dark_mode"), "Enable Dark Mode"),
                    tags$hr(),
                    tags$h4("Downloads"),
                    downloadButton(ns("download"),        "Download CSV",                class = "btn-block mb-1"),
                    downloadButton(ns("download_excel"),  "Download Excel",              class = "btn-block mb-1"),
                    downloadButton(ns("download_dta"),    "Download Stata",              class = "btn-block mb-1"),
                    downloadButton(ns("download_report"), "Download Risk Report As pdf", class = "btn-block mb-1"),
                    actionButton(ns("view_report"),       "View Report",                 class = "btn btn-info btn-block"),
                    
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'Anonymize Coordinates'", ns_method),
                      div(
                        class = "download-note",
                        HTML("<strong>Note:</strong> When <em>Anonymize Coordinates</em> is the last applied method, the standard downloads above will export the anonymized coordinates. No separate download is needed.")
                      )
                    ),
                    
                    tags$hr(),
                    uiOutput(ns("k_report")),
                    tags$hr(),
                    tags$h4("Steps Log"),
                    verbatimTextOutput(ns("step_log"), placeholder = TRUE)
                  )
                )
              ),
              
              # ---------- RIGHT PANEL ----------
              column(
                width = 8, id = ns_right_panel,
                div(
                  class = "right-containers",
                  
                  # === Container 1: Data Preview / Map ===
                  div(
                    class = "right-box",
                    div(class = "right-header", tags$h3("Data Preview")),
                    div(
                      class = "right-body",
                      conditionalPanel(
                        condition = sprintf("input['%s'] == 'Anonymize Coordinates'", ns_method),
                        leaflet::leafletOutput(ns("geo_map"), height = "420px")
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] != 'Anonymize Coordinates'", ns_method),
                        div(
                          id    = ns("preview-table"),
                          class = "table-wrap",
                          tableOutput(ns("preview_merged"))
                        )
                      )
                    )
                  ),
                  
                  # === Container 2: Risk Assessment ===
                  div(
                    class = "right-box",
                    div(class = "right-header", tags$h3("Risk Assessment")),
                    div(
                      class = "right-body",
                      div(
                        class = "risk-summary-grid",
                        div(class = "summary-card", uiOutput(ns("risk_before"))),
                        div(class = "summary-card", uiOutput(ns("risk_after")))
                      ),
                      tags$br(),
                      div(
                        id = ns("preview-gauges"),
                        fluidRow(
                          column(
                            width = 6,
                            div(
                              class = "gauge-box",
                              tags$h4("Risk Before"),
                              flexdashboard::gaugeOutput(ns("gauge_before"), height = "200px")
                            )
                          ),
                          column(
                            width = 6,
                            div(
                              class = "gauge-box",
                              tags$h4("Risk After"),
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
          tabPanel(
            "Codes",
            fluidRow(
              column(
                width = 4,
                tags$h4("R Code"),
                shinyAce::aceEditor(
                  outputId = ns_r_ace,
                  mode     = "r",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                actionButton(
                  ns_copy_r, "📋 Copy",
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s')", ns_r_ace, ns_copy_r)
                )
              ),
              column(
                width = 4,
                tags$h4("Stata Code"),
                shinyAce::aceEditor(
                  outputId = ns_stata_ace,
                  mode     = "stata",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                actionButton(
                  ns_copy_st, "📋 Copy",
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s')", ns_stata_ace, ns_copy_st)
                )
              ),
              column(
                width = 4,
                tags$h4("Python Code"),
                shinyAce::aceEditor(
                  outputId = ns_py_ace,
                  mode     = "python",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                actionButton(
                  ns_copy_py, "📋 Copy",
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s')", ns_py_ace, ns_copy_py)
                )
              )
            )
          ),
          
          # -------------------------- DESCRIPTIONS TAB ----------------------------
          tabPanel(
            "Descriptions",
            uiOutput(ns("descriptions_panel"))
          )
        )
      )
    )
  )
}

mod_quant_anon_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Call the Anon server logic function if it exists
    if (exists("anon_quant_server_logic", mode = "function", inherits = TRUE)) {
      get("anon_quant_server_logic", mode = "function", inherits = TRUE)(input, output, session)
    } else {
      showNotification(
        "anon_quant_server_logic() not found. Check Anon/server_module_quant.R",
        type = "error"
      )
    }
    
  })
}
