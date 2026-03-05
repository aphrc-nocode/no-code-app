homepage <- function(){
  tabItem(
    tabName = "homepage",
    fluidRow(
      class  = "aphrc-row2",
      
      tags$head(
        tags$style(HTML("
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

/* -----------------------------
   Global typography (match old)
------------------------------*/
html, body{
  font-family: \"Inter\", \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  color: var(--aphrc-ink);
}

/* IMPORTANT: Shiny textOutput inserts .shiny-text-output elements.
   Force them to inherit typography and not behave like block divs. */
.aphrc-container .shiny-text-output,
.aphrc-container .shiny-html-output{
  display: inline;
  font: inherit;
  color: inherit;
  line-height: inherit;
  margin: 0;
  padding: 0;
}

/* -----------------------------
   Outer container (match old)
------------------------------*/
.aphrc-container{
  max-width: 980px;
  margin: 10px auto;
  background: var(--aphrc-bg);
  border: 2px solid var(--aphrc-green);
  border-radius: 18px;
  padding: 28px 28px 32px;
  box-shadow: var(--aphrc-shadow);
  font-size: 18px;
}

/* -----------------------------
   Top title + intro area
------------------------------*/
.aphrc-title{
  margin: 0 0 6px;
  font-size: 42px;
  line-height: 1.08;
  color: var(--aphrc-ink);
  letter-spacing: .2px;
  font-weight: 400; /* closer to original look */
}

/* HR lines like the original */
.aphrc-hr{
  border: 0;
  height: 4px;
  margin: 14px 0 18px;
  background: linear-gradient(90deg, var(--aphrc-green), var(--aphrc-teal));
  border-radius: 999px;
  opacity: .95;
}

/* Intro box (original feel: soft, roomy, teal border) */
.aphrc-intro{
  margin: 0;
  font-size: 19px;
  line-height: 1.85;
  color: var(--aphrc-ink);
  background: #f3fbf3;                 /* flatter like old */
  padding: 18px 20px;
  border-left: 6px solid var(--aphrc-teal);
  border-radius: 10px;
}

/* Link style (match old screenshot) */
.aphrc-link{
  display: inline-block;
  margin-top: 12px;
  font-weight: 500;
  color: var(--aphrc-teal);
  text-decoration: none;
  font-size: 13px;
}
.aphrc-link:hover{
  color: var(--aphrc-teal);
  text-decoration: underline;
}

/* -----------------------------
   Panels (Objectives + Components)
------------------------------*/
.aphrc-panel{
  margin: 18px 0 0;
  background: #f6fbf6;
  border: 1px solid #e5efe5;
  border-radius: 14px;
  overflow: hidden;
  box-shadow: var(--aphrc-shadow2);
}

/* Old design has a soft grey interior */
.aphrc-panel-body{
  padding: 26px 22px 20px;
  background: #f2f2f2;
}

/* Panel caption strip */
.aphrc-panel-caption{
  padding: 10px 14px;
  font-size: 12px;
  color: var(--aphrc-ink);
  border-top: 1px solid #e9e9e9;
  background: #f6fbf6;
}

/* -----------------------------
   Section headings
------------------------------*/
.aphrc-section-kicker{
  text-align: center;
  font-size: 10px;
  letter-spacing: .40em;
  color: #7a8f7f;
  margin: 6px 0 6px;
}

.aphrc-section-title{
  text-align: center;
  margin: 0 0 10px;
  font-size: 34px;
  color: #3f3f3f;     /* darker grey like old */
  font-weight: 800;
}

/* The little tricolor underline */
.aphrc-tricolor{
  width: 110px;
  height: 4px;
  border-radius: 999px;
  margin: 0 auto 18px;
  background: linear-gradient(90deg,#e53935 0 33%, #f6a100 33% 66%, #2e7d32 66% 100%);
}

/* -----------------------------
   Grid + Cards
------------------------------*/
.aphrc-grid-3{
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 22px;
}

/* Default card shell */
.aphrc-card{
  background: var(--aphrc-card);
  border: 1px solid #e2e2e2;
  border-radius: 10px;                 /* less rounded like old */
  padding: 18px 18px 16px;
  box-shadow: none;                    /* flatter like old */
}

/* Objective cards in old screenshot are grey/flat */
.aphrc-card.aphrc-obj-1,
.aphrc-card.aphrc-obj-2,
.aphrc-card.aphrc-obj-3{
  background: #f4f4f4;
  border-color: #e0e0e0;
}

/* Objective titles look bold and compact */
.aphrc-card h4{
  margin: 0 0 10px;
  font-size: 16px;
  font-weight: 800;
  color: #3f3f3f;
}

/* Objective underline bars */
.aphrc-card .aphrc-underline{
  height: 3px;
  width: 56%;
  border-radius: 999px;
  margin: 8px 0 14px;
  background: #dedede;
}
.aphrc-obj-1 .aphrc-underline{ background:#f6a100; }
.aphrc-obj-2 .aphrc-underline{ background:#e53935; }
.aphrc-obj-3 .aphrc-underline{ background:#2e7d32; }

/* Bullet styling (close to original spacing) */
.aphrc-bullets{
  margin: 0;
  padding-left: 18px;
  color: #3d3d3d;
  line-height: 1.75;
  font-size: 13px;
}
.aphrc-bullets li{ margin: 10px 0; }
.aphrc-bullets li::marker{ color:#f6a100; }
.aphrc-obj-2 .aphrc-bullets li::marker{ color:#e53935; }
.aphrc-obj-3 .aphrc-bullets li::marker{ color:#2e7d32; }

/* Highlight objective card stays green (as in original) */
.aphrc-card.aphrc-highlight{
  background:#08a84a;
  border-color:#08a84a;
  color:#fff;
}
.aphrc-card.aphrc-highlight h4{ color:#fff; }
.aphrc-card.aphrc-highlight .aphrc-bullets{ color:#fff; }
.aphrc-card.aphrc-highlight .aphrc-bullets li::marker{ color:#d9ffe6; }
.aphrc-card.aphrc-highlight .aphrc-underline{ background: rgba(255,255,255,0.35); }

/* -----------------------------
   Components (icons + text)
------------------------------*/
.aphrc-icon{
  width: 62px;
  height: 62px;
  border-radius: 999px;
  display: grid;
  place-items: center;
  margin: 0 auto 12px;
  color: #fff;
  font-weight: 900;
  font-size: 24px;
}
.aphrc-icon.red{ background:#e53935; }
.aphrc-icon.amber{ background:#f6a100; }
.aphrc-icon.green{ background:#2e7d32; }

.aphrc-comp-title{
  text-align: center;
  margin: 0 0 8px;
  font-size: 14px;
  font-weight: 800;
  color: #3f3f3f;
}

.aphrc-comp-underline{
  height: 2px;
  width: 58%;
  border-radius: 999px;
  margin: 10px auto 14px;
  background: #e0e0e0;
}
.aphrc-comp-underline.red{ background:#e53935; }
.aphrc-comp-underline.amber{ background:#f6a100; }
.aphrc-comp-underline.green{ background:#2e7d32; }

.aphrc-comp-desc{
  margin: 0;
  color: #3d3d3d;
  line-height: 1.7;
  font-size: 12px;
  text-align: left;
}

/* -----------------------------
   Responsive
------------------------------*/
@media (max-width: 860px){
  .aphrc-grid-3{ grid-template-columns: 1fr; }
  .aphrc-container{
    padding: 20px;
    font-size: 16px;
  }
  .aphrc-title{ font-size: 38px; }
  .aphrc-section-title{ font-size: 26px; }
  .aphrc-intro{ font-size: 16px; }
}
  "))
      ),
      
      # Content (container preserved to keep layout stable)
      tags$div(
        class = "aphrc-container",
        
        tags$h1(class = "aphrc-title", textOutput("homepage_title", container = span)),
        tags$hr(class = "aphrc-hr"),
        
        tags$p(class = "aphrc-intro", textOutput("homepage_intro", container = span)),
        uiOutput("homepage_more_details_link"),
        
        tags$hr(class = "aphrc-hr"),
        
        # ======= Panel 1: OBJECTIVES =======
        tags$div(
          class="aphrc-panel",
          tags$div(
            class="aphrc-panel-body",
            
            tags$div(class="aphrc-section-kicker", textOutput("homepage_objectives_kicker", container = span)),
            tags$div(class="aphrc-section-title", textOutput("homepage_objectives_title", container = span)),
            tags$div(class="aphrc-tricolor"),
            
            tags$div(
              class="aphrc-grid-3",
              
              tags$div(
                class="aphrc-card aphrc-obj-1",
                tags$h4(textOutput("homepage_obj1_title", container = span)),
                tags$div(class="aphrc-underline"),
                tags$ul(
                  class="aphrc-bullets",
                  tags$li(textOutput("homepage_obj1_bullet1", container = span)),
                  tags$li(textOutput("homepage_obj1_bullet2", container = span))
                )
              ),
              
              tags$div(
                class="aphrc-card aphrc-obj-2",
                tags$h4(textOutput("homepage_obj2_title", container = span)),
                tags$div(class="aphrc-underline"),
                tags$ul(
                  class="aphrc-bullets",
                  tags$li(textOutput("homepage_obj2_bullet1", container = span)),
                  tags$li(textOutput("homepage_obj2_bullet2", container = span))
                )
              ),
              
              tags$div(
                class="aphrc-card aphrc-obj-3 aphrc-highlight",
                tags$h4(textOutput("homepage_obj3_title", container = span)),
                tags$div(class="aphrc-underline"),
                tags$ul(
                  class="aphrc-bullets",
                  tags$li(textOutput("homepage_obj3_bullet1", container = span)),
                  tags$li(textOutput("homepage_obj3_bullet2", container = span))
                )
              )
            )
          ),
          tags$div(class="aphrc-panel-caption", textOutput("homepage_platform_overview_caption", container = span))
        ),
        
        tags$hr(class="aphrc-hr"),
        
        # ======= Panel 2: COMPONENTS =======
        tags$div(
          class="aphrc-panel",
          tags$div(
            class="aphrc-panel-body",
            
            tags$div(class="aphrc-section-title", textOutput("homepage_components_title", container = span)),
            tags$div(class="aphrc-tricolor"),
            
            tags$div(
              class="aphrc-grid-3",
              
              tags$div(
                class="aphrc-card",
                tags$div(class="aphrc-icon red", "⦿"),
                tags$div(class="aphrc-comp-title", textOutput("homepage_comp1_title", container = span)),
                tags$div(class="aphrc-comp-underline red"),
                tags$p(class="aphrc-comp-desc", textOutput("homepage_comp1_desc", container = span))
              ),
              
              tags$div(
                class="aphrc-card",
                tags$div(class="aphrc-icon amber", "⚙"),
                tags$div(class="aphrc-comp-title", textOutput("homepage_comp2_title", container = span)),
                tags$div(class="aphrc-comp-underline amber"),
                tags$p(class="aphrc-comp-desc", textOutput("homepage_comp2_desc", container = span))
              ),
              
              tags$div(
                class="aphrc-card",
                tags$div(class="aphrc-icon green", "⇪"),
                tags$div(class="aphrc-comp-title", textOutput("homepage_comp3_title", container = span)),
                tags$div(class="aphrc-comp-underline green"),
                tags$p(class="aphrc-comp-desc", textOutput("homepage_comp3_desc", container = span))
              )
            )
          ),
          tags$div(class="aphrc-panel-caption", textOutput("homepage_end_to_end_workflow_caption", container = span))
        )
      )
    )
  )
}