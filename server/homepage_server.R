# server/homepage_server.R

homepage_server <- function(output, session) {
  
  output$homepage_ui <- shiny::renderUI({
    
    # Local helper: safe label fetch + fallback
    .lbl <- function(key, default = key) {
      val <- tryCatch(as.character(get_rv_labels(key)), error = function(e) "")
      val <- trimws(val)
      if (is.null(val) || length(val) == 0 || is.na(val) || !nzchar(val)) default else val
    }
    
    shinydashboard::tabItem(
      tabName = "homepage",
      shiny::fluidRow(
        class  = "aphrc-row2",
        
        shiny::tags$head(
          shiny::tags$style(shiny::HTML("
:root{
  --aphrc-green:#7BC148;
  --aphrc-teal:#00BFC4;
  --aphrc-ink:#083B3F;
  --aphrc-bg:#F7FFF7;
  --aphrc-muted:#e8f5e8;
  --aphrc-card:#ffffff;
  --aphrc-border:#e6efe6;
  --aphrc-shadow: 0 10px 24px rgba(0,0,0,0.06);
  --aphrc-shadow2: 0 8px 18px rgba(0,0,0,0.05);
}

.aphrc-container{
  max-width: 980px;
  margin:10px auto;
  background: var(--aphrc-bg);
  border:2px solid var(--aphrc-green);
  border-radius:18px;
  padding:28px 28px 32px;
  box-shadow:var(--aphrc-shadow);
}

.aphrc-title{
  margin:0 0 6px;
  font-size:3rem;
  line-height:1.15;
  color:var(--aphrc-ink);
  letter-spacing:.2px;
}

.aphrc-hr{
  border:0;
  height:4px;
  margin:14px 0 18px;
  background:linear-gradient(90deg,var(--aphrc-green),var(--aphrc-teal));
  border-radius:999px;
  opacity:.95;
}

.aphrc-intro{
  margin:0;
  font-size:1.15rem;
  line-height:1.65;
  color:var(--aphrc-ink);
  background:linear-gradient(0deg,#fff,var(--aphrc-muted));
  padding:14px 16px;
  border-left:5px solid var(--aphrc-teal);
  border-radius:10px;
}

.aphrc-link{
  display:inline-block;
  margin-top:10px;
  font-weight:600;
  color:var(--aphrc-teal);
  text-decoration:none;
  transition:color .25s ease, transform .25s ease;
}
.aphrc-link:hover{
  color:var(--aphrc-green);
  transform:translateY(-2px);
  text-decoration:underline;
}

.aphrc-panel{
  margin:16px 0 0;
  background:#fff;
  border:1px solid var(--aphrc-border);
  border-radius:14px;
  overflow:hidden;
  box-shadow:var(--aphrc-shadow2);
}
.aphrc-panel-body{ padding:18px 18px 16px; }
.aphrc-panel-caption{
  padding:10px 14px;
  font-size:.95rem;
  color:var(--aphrc-ink);
  border-top:1px solid #eef6ee;
  background:linear-gradient(0deg,var(--aphrc-bg),#fff);
}

.aphrc-section-kicker{
  text-align:center;
  font-size:.75rem;
  letter-spacing:.35em;
  color:#6f8f76;
  margin:6px 0 6px;
}
.aphrc-section-title{
  text-align:center;
  margin:0 0 10px;
  font-size:2rem;
  color:#4a4a4a;
  font-weight:800;
}
.aphrc-tricolor{
  width:110px;
  height:4px;
  border-radius:999px;
  margin:0 auto 14px;
  background:linear-gradient(90deg,#e53935 0 33%, #f6a100 33% 66%, #2e7d32 66% 100%);
}

.aphrc-grid-3{
  display:grid;
  grid-template-columns:repeat(3, minmax(0, 1fr));
  gap:18px;
}

.aphrc-card{
  background:var(--aphrc-card);
  border:1px solid var(--aphrc-border);
  border-radius:12px;
  padding:16px 16px 14px;
  box-shadow:0 6px 14px rgba(0,0,0,0.04);
}
.aphrc-card h4{
  margin:0 0 10px;
  font-size:1rem;
  font-weight:800;
  color:#4a4a4a;
}
.aphrc-card .aphrc-underline{
  height:3px;
  width:62%;
  border-radius:999px;
  margin:8px 0 12px;
  background:#e0e0e0;
}

.aphrc-obj-1 .aphrc-underline{ background:#f6a100; }
.aphrc-obj-2 .aphrc-underline{ background:#e53935; }
.aphrc-obj-3 .aphrc-underline{ background:#2e7d32; }

.aphrc-bullets{
  margin:0;
  padding-left:18px;
  color:#3d3d3d;
  line-height:1.55;
}
.aphrc-bullets li{ margin:8px 0; }
.aphrc-bullets li::marker{ color:#f6a100; }
.aphrc-obj-2 .aphrc-bullets li::marker{ color:#e53935; }
.aphrc-obj-3 .aphrc-bullets li::marker{ color:#2e7d32; }

.aphrc-card.aphrc-highlight{
  background:#08a84a;
  border-color:#08a84a;
  color:#fff;
}
.aphrc-card.aphrc-highlight h4{ color:#fff; }
.aphrc-card.aphrc-highlight .aphrc-bullets{ color:#fff; }
.aphrc-card.aphrc-highlight .aphrc-bullets li::marker{ color:#d9ffe6; }
.aphrc-card.aphrc-highlight .aphrc-underline{ background:rgba(255,255,255,0.35); }

.aphrc-icon{
  width:58px; height:58px;
  border-radius:999px;
  display:grid;
  place-items:center;
  margin:0 auto 10px;
  color:#fff;
  font-weight:900;
  font-size:22px;
}
.aphrc-icon.red{ background:#e53935; }
.aphrc-icon.amber{ background:#f6a100; }
.aphrc-icon.green{ background:#2e7d32; }

.aphrc-comp-title{
  text-align:center;
  margin:0 0 10px;
  font-size:1.05rem;
  font-weight:800;
  color:#4a4a4a;
}
.aphrc-comp-underline{
  height:2px;
  width:58%;
  border-radius:999px;
  margin:8px auto 12px;
  background:#e0e0e0;
}
.aphrc-comp-underline.red{ background:#e53935; }
.aphrc-comp-underline.amber{ background:#f6a100; }
.aphrc-comp-underline.green{ background:#2e7d32; }

.aphrc-comp-desc{
  margin:0;
  color:#3d3d3d;
  line-height:1.6;
  font-size:.98rem;
  text-align:left;
}

@media (max-width: 860px){
  .aphrc-grid-3{ grid-template-columns:1fr; }
  .aphrc-title{ font-size:2rem; }
  .aphrc-container{ padding:20px; }
}
          "))
        ),
        
        shiny::tags$div(
          class = "aphrc-container",
          
          shiny::tags$h1(class="aphrc-title", .lbl("homepage_title")),
          shiny::tags$hr(class="aphrc-hr"),
          
          shiny::tags$p(class="aphrc-intro", .lbl("homepage_intro")),
          
          shiny::tags$a(
            href   = .lbl("homepage_more_details_url", "#"),
            target = "_blank",
            class  = "aphrc-link",
            .lbl("homepage_more_details_link_text")
          ),
          
          shiny::tags$hr(class="aphrc-hr"),
          
          shiny::tags$div(
            class="aphrc-panel",
            shiny::tags$div(
              class="aphrc-panel-body",
              shiny::tags$div(class="aphrc-section-kicker", .lbl("homepage_objectives_kicker")),
              shiny::tags$div(class="aphrc-section-title",  .lbl("homepage_objectives_title")),
              shiny::tags$div(class="aphrc-tricolor"),
              
              shiny::tags$div(
                class="aphrc-grid-3",
                
                shiny::tags$div(
                  class="aphrc-card aphrc-obj-1",
                  shiny::tags$h4(.lbl("homepage_obj1_title")),
                  shiny::tags$div(class="aphrc-underline"),
                  shiny::tags$ul(
                    class="aphrc-bullets",
                    shiny::tags$li(.lbl("homepage_obj1_bullet1")),
                    shiny::tags$li(.lbl("homepage_obj1_bullet2"))
                  )
                ),
                
                shiny::tags$div(
                  class="aphrc-card aphrc-obj-2",
                  shiny::tags$h4(.lbl("homepage_obj2_title")),
                  shiny::tags$div(class="aphrc-underline"),
                  shiny::tags$ul(
                    class="aphrc-bullets",
                    shiny::tags$li(.lbl("homepage_obj2_bullet1")),
                    shiny::tags$li(.lbl("homepage_obj2_bullet2"))
                  )
                ),
                
                shiny::tags$div(
                  class="aphrc-card aphrc-obj-3 aphrc-highlight",
                  shiny::tags$h4(.lbl("homepage_obj3_title")),
                  shiny::tags$div(class="aphrc-underline"),
                  shiny::tags$ul(
                    class="aphrc-bullets",
                    shiny::tags$li(.lbl("homepage_obj3_bullet1")),
                    shiny::tags$li(.lbl("homepage_obj3_bullet2"))
                  )
                )
              )
            ),
            shiny::tags$div(class="aphrc-panel-caption", .lbl("homepage_platform_overview_caption"))
          ),
          
          shiny::tags$hr(class="aphrc-hr"),
          
          shiny::tags$div(
            class="aphrc-panel",
            shiny::tags$div(
              class="aphrc-panel-body",
              shiny::tags$div(class="aphrc-section-title", .lbl("homepage_components_title")),
              shiny::tags$div(class="aphrc-tricolor"),
              
              shiny::tags$div(
                class="aphrc-grid-3",
                
                shiny::tags$div(
                  class="aphrc-card",
                  shiny::tags$div(class="aphrc-icon red", "⦿"),
                  shiny::tags$div(class="aphrc-comp-title", .lbl("homepage_comp1_title")),
                  shiny::tags$div(class="aphrc-comp-underline red"),
                  shiny::tags$p(class="aphrc-comp-desc", .lbl("homepage_comp1_desc"))
                ),
                
                shiny::tags$div(
                  class="aphrc-card",
                  shiny::tags$div(class="aphrc-icon amber", "⚙"),
                  shiny::tags$div(class="aphrc-comp-title", .lbl("homepage_comp2_title")),
                  shiny::tags$div(class="aphrc-comp-underline amber"),
                  shiny::tags$p(class="aphrc-comp-desc", .lbl("homepage_comp2_desc"))
                ),
                
                shiny::tags$div(
                  class="aphrc-card",
                  shiny::tags$div(class="aphrc-icon green", "⇪"),
                  shiny::tags$div(class="aphrc-comp-title", .lbl("homepage_comp3_title")),
                  shiny::tags$div(class="aphrc-comp-underline green"),
                  shiny::tags$p(class="aphrc-comp-desc", .lbl("homepage_comp3_desc"))
                )
              )
            ),
            
            # Your Excel doesn't include homepage_end_to_end_caption; fallback
            shiny::tags$div(
              class="aphrc-panel-caption",
              .lbl("homepage_end_to_end_caption", .lbl("homepage_platform_overview_caption", ""))
            )
          )
        )
      )
    )
  })
  
  shiny::outputOptions(output, "homepage_ui", suspendWhenHidden = FALSE)
}