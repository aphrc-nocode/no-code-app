homepage <- function(){
    tabItem(tabName = "homepage",
            fluidRow(class  = "aphrc-row2",
                     
                     tags$head(
                       # Inline CSS (you can move this to www/aphrc.css and use includeCSS)
                       tags$style(HTML("
      :root{
        --aphrc-green:#7BC148; --aphrc-teal:#00BFC4; --aphrc-ink:#083B3F;
        --aphrc-bg:#F7FFF7; --aphrc-muted:#e8f5e8;
      }
      .aphrc-container{
        max-width: 980px; margin:10px auto; background: var(--aphrc-bg);
        border:2px solid var(--aphrc-green); border-radius:18px; padding:28px 28px 32px;
        box-shadow:0 10px 24px rgba(0,0,0,0.06);
      }
      .aphrc-title{ margin:0 0 px; font-size:4rem; line-height:1.15;
        color:var(--aphrc-ink); letter-spacing:.2px; }
      .aphrc-hr{ border:0; height:4px; margin:14px 0 18px;
        background:linear-gradient(90deg,var(--aphrc-green),var(--aphrc-teal));
        border-radius:999px; opacity:.95; }
      .aphrc-intro{
        margin:0; font-size:2rem; line-height:1.6; color:var(--aphrc-ink);
        background:linear-gradient(0deg,#fff,var(--aphrc-muted));
        padding:14px 16px; border-left:5px solid var(--aphrc-teal); border-radius:10px;
      }
        .aphrc-link{
        display:inline-block; margin-top:12px; font-weight:600;
        color:var(--aphrc-teal); text-decoration:none;
        transition:color .3s ease, transform .3s ease;
        margin:3px
      }
      .aphrc-link:hover{
        color:var(--aphrc-green);
        transform:translateY(-2px);
        text-decoration:underline;
      }
      .aphrc-figure{ margin:0; background:#fff; border:1px solid #e6efe6;
        border-radius:14px; overflow:hidden; box-shadow:0 8px 18px rgba(0,0,0,0.05); }
      .aphrc-img{ width:100%; height:auto; display:block; }
      .aphrc-figure figcaption{
        padding:10px 14px; font-size:.95rem; color:var(--aphrc-ink);
        border-top:1px solid #eef6ee; background:linear-gradient(0deg,var(--aphrc-bg),#fff);
      }
      @media (max-width:640px){ .aphrc-container{padding:20px;} .aphrc-title{font-size:1.6rem;} }
    "))
                     ),
                     
                     # Content
                     tags$div(class = "aphrc-container",
                              tags$h1(class = "aphrc-title", "No-Code Platform Development"),
                              tags$hr(class = "aphrc-hr"),
                              
                              tags$p(class = "aphrc-intro",
                                     "This platform is designed to help users navigate through the end-to-end predictive analytics ",
                                     "using AI/ML tools, without the need for programming expertise. It features a point-and-click ",
                                     "interface with user-guided feedback, and is specifically tailored for public health researchers."
                              ),
                              
                              tags$a(href = "https://dswb.africa/",
                                     target = "_blank",
                                     class = "aphrc-link",
                                     "For more details please access Data Science Without Borders page"),
                              
                              tags$p(class = "aphrc-intro",
                              tags$hr(class = "aphrc-hr"),
                              
                              # First image
                              tags$figure(class = "aphrc-figure",
                                          tags$img(src = "homepage2.png", alt = "Platform overview", class = "aphrc-img"),
                                          tags$figcaption("Platform Overview")
                              ),
                              
                              tags$hr(class = "aphrc-hr"),
                              
                              # Last image
                              tags$figure(class = "aphrc-figure",
                                          tags$img(src = "homepage1.png", alt = "End-to-End workflow", class = "aphrc-img"),
                                          tags$figcaption("End-to-End Workflow")
                              )
                     )    

                     
            )
))
}