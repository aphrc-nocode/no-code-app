header = tags$div(
  class = "custom-header",
  style = "background-color: #7BC148; color: white; padding: 10px;",
  # Dropdown CSS — Vercel/Stripe-inspired, sharp and minimal
  tags$style(HTML("
    .user-dropdown-wrap { position: relative; cursor: pointer; user-select: none; }
    .user-dropdown-btn  {
      display: flex; align-items: center; gap: 6px;
      padding: 5px 10px; border-radius: 6px;
      transition: background .15s ease;
      font-size: 13px; font-weight: 500; letter-spacing: .01em;
    }
    .user-dropdown-btn:hover { background: rgba(255,255,255,0.12); }
    .user-dropdown-btn .fa-chevron-down {
      font-size: 9px; opacity: .6; margin-left: 2px;
      transition: transform .2s ease;
    }
    .user-dropdown-wrap.open .fa-chevron-down { transform: rotate(180deg); }
    .user-dropdown-menu {
      display: none; position: absolute; right: 0; top: calc(100% + 6px);
      min-width: 170px; background: #fff; border-radius: 8px;
      border: 1px solid #e5e5e5;
      box-shadow: 0 2px 8px rgba(0,0,0,.08), 0 0 1px rgba(0,0,0,.1);
      z-index: 9999; overflow: hidden;
      animation: ddFadeIn .12s ease;
    }
    @keyframes ddFadeIn {
      from { opacity: 0; transform: translateY(-4px); }
      to   { opacity: 1; transform: translateY(0); }
    }
    .user-dropdown-wrap.open .user-dropdown-menu { display: block; }
    .user-dropdown-menu .dd-divider {
      height: 1px; background: #f0f0f0; margin: 4px 0;
    }
    .user-dropdown-menu .dd-item {
      display: flex; align-items: center; gap: 8px;
      padding: 8px 14px; color: #444; font-size: 13px; font-weight: 450;
      cursor: pointer; transition: all .12s ease;
      letter-spacing: .01em;
    }
    .user-dropdown-menu .dd-item i { font-size: 13px; color: #888; width: 16px; text-align: center; }
    .user-dropdown-menu .dd-item:hover { background: #fafafa; color: #111; }
    .user-dropdown-menu .dd-item:hover i { color: #555; }
    .user-dropdown-menu .dd-item.logout:hover { background: #fef2f2; color: #dc2626; }
    .user-dropdown-menu .dd-item.logout:hover i { color: #dc2626; }
  ")),
  # Close dropdown when clicking elsewhere
  tags$script(HTML("
    $(document).on('click', '.user-dropdown-wrap', function(e) {
      e.stopPropagation();
      $(this).toggleClass('open');
    });
    $(document).on('click', function() {
      $('.user-dropdown-wrap').removeClass('open');
    });
  ")),
  fluidRow(
    column(2, tags$img(src = "aphrc.png", height = "50px")),
    column(6, uiOutput("app_title", align="right")),
    column(2, uiOutput("change_language")),
    column(2, align="right",
           login::is_logged_in(
             id = app_login_config$APP_ID,
             style="display: flex; justify-content: flex-end; gap: 15px; margin-top: 20px;",
             br(),
             tags$div(
               class = "user-dropdown-wrap",
               # Clickable trigger
               tags$div(
                 class = "user-dropdown-btn",
                 icon("user", style = "font-size: 14px; color: white;"),
                 uiOutput("userName", inline = TRUE),
                 icon("chevron-down", style = "color: white;")
               ),
               # Dropdown
               tags$div(
                 class = "user-dropdown-menu",
                 tags$div(
                   class = "dd-item",
                   onclick = "Shiny.setInputValue('show_profile', Math.random())",
                   icon("user"), "Profile"
                 ),
                 tags$div(class = "dd-divider"),
                 tags$div(
                   class = "dd-item logout",
                   onclick = "Shiny.setInputValue('logoutID', Math.random(), {priority: 'event'})",
                   icon("power-off"), "Sign out"
                 )
               )
             )
           )
    )
  ))
