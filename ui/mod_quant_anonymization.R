# modules/mod_quant_anonymization.R

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
  
  ns_dashboard <- ns("dashboard")
  
  # Hidden translation “tokens” for JS
  js_i18n_tokens <- shiny::tags$div(
    style = "display:none;",
    shiny::tags$span(id = ns("js_copy_txt"),   shiny::textOutput(ns("quant_anon_js_copy_txt"),   container = shiny::span)),
    shiny::tags$span(id = ns("js_copied_txt"), shiny::textOutput(ns("quant_anon_js_copied_txt"), container = shiny::span))
  )
  
  shiny::div(
    id    = ns("anon_root"),
    class = "anon-root",
    shiny::tagList(
      shinyjs::useShinyjs(),
      
      shiny::tags$head(
        shiny::tags$link(
          rel  = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
        ),
        
        shiny::tags$script(shiny::HTML(sprintf("
          function copyAce(editorId, btnId, nsPrefix) {
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
              var copiedEl = document.getElementById(nsPrefix + '-js_copied_txt');
              var copyEl   = document.getElementById(nsPrefix + '-js_copy_txt');

              var copiedTxt = copiedEl ? copiedEl.innerText : 'Copied!';
              var copyTxt   = copyEl   ? copyEl.innerText   : '📋 Copy';

              btn.innerText = copiedTxt;
              setTimeout(function(){ btn.innerText = copyTxt; }, 2000);
            }
          }
        ")))
      ),
      
      js_i18n_tokens,
      
      shiny::div(
        id    = ns_dashboard,
        class = "anon-dashboard",
        style = "display:block; padding:20px;",
        
        shiny::tabsetPanel(
          id = ns_main_tabs,
          
          # -------------------------- DASHBOARD TAB -------------------------------
          shiny::tabPanel(
            shiny::uiOutput(ns("quant_anon_tab_dashboard")),
            
            shiny::fluidRow(
              # ---------- LEFT PANEL ----------
              shiny::column(
                width = 4, id = ns("left-panel"),
                
                # Step 0
                shiny::wellPanel(
                  shiny::uiOutput(ns("quant_anon_step0_title_ui")),
                  
                  shiny::div(
                    style = "padding:8px; border:1px solid #eee; border-radius:6px;",
                    shiny::uiOutput(ns("quant_anon_step0_use_platform_title_ui")),
                    shiny::br(),
                    shiny::uiOutput(ns("platform_dataset_picker")),
                    
                    shiny::actionButton(
                      ns("use_platform_data"),
                      label = NULL, # Server controls label with updateActionButton()
                      class = "btn btn-success btn-block"
                    ),
                    
                    shiny::tags$small(
                      style = "display:block; margin-top:6px; color:#666;",
                      shiny::uiOutput(ns("quant_anon_step0_hint_ui"))
                    )
                  ),
                  
                  shiny::tags$hr(),
                  shiny::textOutput(ns("n_obs_text"))
                ),
                
                # Step 1
                shiny::wellPanel(
                  shiny::div(
                    class = "qid-header",
                    shiny::uiOutput(ns("quant_anon_step1_title_ui")),
                    shiny::tags$button(
                      id      = ns_remove_inline,
                      class   = "icon-button",
                      title   = NULL,
                      onclick = sprintf("$('#%s').click();", ns_remove_ids),
                      shiny::tags$i(class = "fa fa-eraser"),
                      shiny::uiOutput(ns("quant_anon_remove_identifiers_tooltip_attr_ui"))
                    )
                  ),
                  shiny::uiOutput(ns("identifier_selector"))
                ),
                
                # Step 2
                shiny::wellPanel(
                  shiny::uiOutput(ns("quant_anon_step2_title_ui")),
                  shiny::uiOutput(ns("bucket_ui"))
                ),
                
                # Step 3
                shiny::wellPanel(
                  shiny::uiOutput(ns("quant_anon_step3_title_ui")),
                  
                  shiny::selectInput(
                    ns_method,
                    label    = NULL,        #  server sets label via updateSelectInput()
                    choices  = character(0),
                    selected = NULL
                  ),
                  
                  shiny::uiOutput(ns("extra_input")),
                  
                  #  IMPORTANT: compare against stable code
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'generalization'", ns_method),
                    shiny::uiOutput(ns("quant_anon_generalization_tips_ui"))
                  ),
                  
                  shiny::fluidRow(
                    shiny::column(
                      4,
                      shiny::actionButton(
                        ns("apply"),
                        label = NULL,
                        class = "btn btn-primary btn-block"
                      )
                    ),
                    shiny::column(
                      4,
                      shiny::actionButton(
                        ns("undo"),
                        label = NULL, 
                        class = "btn btn-warning btn-block"
                      )
                    ),
                    shiny::column(
                      4,
                      shiny::actionButton(
                        ns("reset"),
                        label = NULL,
                        class = "btn btn-danger btn-block"
                      )
                    )
                  ),
                  
                  # Advisor
                  shiny::wellPanel(
                    shiny::uiOutput(ns("quant_anon_advisor_title_ui")),
                    
                    shiny::selectInput(
                      ns("advisor_var"),
                      label     = NULL, # 
                      choices   = NULL,
                      selectize = TRUE
                    ),
                    
                    shiny::actionButton(
                      ns("advisor_run"),
                      label = NULL, # server controls
                      class = "btn btn-info btn-block"
                    ),
                    
                    shiny::tags$hr(),
                    shiny::uiOutput(ns("quant_anon_overall_hist_ui")),
                    shiny::plotOutput(ns("advisor_dist"), height = "220px"),
                    shiny::uiOutput(ns("quant_anon_summary_stats_ui")),
                    shiny::verbatimTextOutput(ns("advisor_summary")),
                    shiny::uiOutput(ns("quant_anon_binwidth_suggestions_ui")),
                    shiny::tableOutput(ns("advisor_table")),
                    shiny::plotOutput(ns("advisor_plot"), height = "220px")
                  ),
                  
                  # Downloads / Report
                  shiny::wellPanel(
                    shiny::checkboxInput(
                      ns("dark_mode"),
                      label = NULL, # 
                      value = FALSE
                    ),
                    
                    shiny::tags$hr(),
                    shiny::uiOutput(ns("quant_anon_downloads_title_ui")),
                    
                    shiny::uiOutput(ns("download_btn_csv_ui")),
                    shiny::uiOutput(ns("download_btn_excel_ui")),
                    shiny::uiOutput(ns("download_btn_dta_ui")),
                    shiny::uiOutput(ns("download_btn_report_ui")),
                    
                    shiny::actionButton(
                      ns("view_report"),
                      label = NULL,
                      class = "btn btn-default btn-block"
                    ),
                    
                    # IMPORTANT: compare against stable code
                    shiny::conditionalPanel(
                      condition = sprintf("input['%s'] == 'anonymizecoordinates'", ns_method),
                      shiny::uiOutput(ns("quant_anon_coords_download_note_ui"))
                    ),
                    
                    shiny::tags$hr(),
                    shiny::uiOutput(ns("k_report")),
                    shiny::tags$hr(),
                    
                    shiny::uiOutput(ns("quant_anon_steps_log_title_ui")),
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
                    shiny::div(
                      class = "right-header",
                      shiny::uiOutput(ns("quant_anon_data_preview_title_ui"))
                    ),
                    shiny::div(
                      class = "right-body",
                      
                      #  IMPORTANT: compare against stable code
                      shiny::conditionalPanel(
                        condition = sprintf("input['%s'] == 'anonymizecoordinates'", ns_method),
                        leaflet::leafletOutput(ns("geo_map"), height = "420px")
                      ),
                      
                      shiny::conditionalPanel(
                        condition = sprintf("input['%s'] != 'anonymizecoordinates'", ns_method),
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
                    shiny::div(
                      class = "right-header",
                      shiny::uiOutput(ns("quant_anon_risk_assessment_title_ui"))
                    ),
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
                              shiny::uiOutput(ns("quant_anon_risk_before_title_ui")),
                              flexdashboard::gaugeOutput(ns("gauge_before"), height = "200px")
                            )
                          ),
                          shiny::column(
                            width = 6,
                            shiny::div(
                              class = "gauge-box",
                              shiny::uiOutput(ns("quant_anon_risk_after_title_ui")),
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
            shiny::uiOutput(ns("quant_anon_tab_codes")),
            
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::uiOutput(ns("quant_anon_r_code_hdr_ui")),
                
                shinyAce::aceEditor(
                  outputId = ns_r_ace,
                  mode     = "r",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                
                shiny::actionButton(
                  ns_copy_r,
                  label   = NULL, #  server controls if desired; or keep static
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s','%s')", ns_r_ace, ns_copy_r, ns("x"))
                )
              ),
              
              shiny::column(
                width = 4,
                shiny::uiOutput(ns("quant_anon_stata_code_hdr_ui")),
                
                shinyAce::aceEditor(
                  outputId = ns_stata_ace,
                  mode     = "stata",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                
                shiny::actionButton(
                  ns_copy_st,
                  label   = NULL,
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s','%s')", ns_stata_ace, ns_copy_st, ns("x"))
                )
              ),
              
              shiny::column(
                width = 4,
                shiny::uiOutput(ns("quant_anon_python_code_hdr_ui")),
                
                shinyAce::aceEditor(
                  outputId = ns_py_ace,
                  mode     = "python",
                  theme    = "chrome",
                  readOnly = TRUE,
                  height   = "400px"
                ),
                
                shiny::actionButton(
                  ns_copy_py,
                  label   = NULL,
                  class   = "btn btn-primary btn-block",
                  onclick = sprintf("copyAce('%s','%s','%s')", ns_py_ace, ns_copy_py, ns("x"))
                )
              )
            )
          ),
          
          # -------------------------- DESCRIPTIONS TAB ----------------------------
          shiny::tabPanel(
            shiny::uiOutput(ns("quant_anon_tab_descriptions")),
            shiny::uiOutput(ns("descriptions_panel"))
          )
        )
      )
    )
  )
}

