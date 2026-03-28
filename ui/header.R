header = tags$div(
  class = "custom-header",
  style = "background-color: #7BC148; color: white; padding: 10px;",
  # Dropdown CSS inline (small, keeps it self-contained)
  tags$style(HTML("
    .user-dropdown-wrap { position: relative; cursor: pointer; }
    .user-dropdown-btn  {
      display: flex; align-items: center; gap: 8px;
      padding: 6px 14px; border-radius: 6px;
      transition: background .2s;
    }
    .user-dropdown-btn:hover { background: rgba(255,255,255,0.15); }
    .user-dropdown-btn .fa-caret-down { font-size: 12px; opacity: .7; transition: transform .2s; }
    .user-dropdown-wrap.open .fa-caret-down { transform: rotate(180deg); }
    .user-dropdown-menu {
      display: none; position: absolute; right: 0; top: 100%; margin-top: 4px;
      min-width: 180px; background: #fff; border-radius: 8px;
      box-shadow: 0 4px 20px rgba(0,0,0,.15); z-index: 9999;
      overflow: hidden;
    }
    .user-dropdown-wrap.open .user-dropdown-menu { display: block; }
    .user-dropdown-menu .dd-header {
      padding: 12px 16px; border-bottom: 1px solid #eee;
      font-size: 11px; color: #999; text-transform: uppercase;
      letter-spacing: .5px;
    }
    .user-dropdown-menu .dd-item {
      display: flex; align-items: center; gap: 10px;
      padding: 10px 16px; color: #333; font-size: 13px;
      cursor: pointer; transition: background .15s;
    }
    .user-dropdown-menu .dd-item:hover { background: #f5f5f5; }
    .user-dropdown-menu .dd-item.logout { color: #e74c3c; }
    .user-dropdown-menu .dd-item.logout:hover { background: #fdf0ef; }
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
               # Clickable button
               tags$div(
                 class = "user-dropdown-btn",
                 icon("user", style = "font-size: 16px; color: white;"),
                 uiOutput("userName", inline = TRUE),
                 icon("caret-down", style = "color: white;")
               ),
               # Dropdown menu
               tags$div(
                 class = "user-dropdown-menu",
                 tags$div(class = "dd-header", "Account"),
                 tags$div(
                   class = "dd-item",
                   onclick = "Shiny.setInputValue('show_profile', Math.random())",
                   icon("user-circle"), "Profile"
                 ),
                 tags$div(
                   class = "dd-item logout",
                   onclick = "Shiny.setInputValue('logoutID', Math.random(), {priority: 'event'})",
                   icon("right-from-bracket"), "Sign out"
                 )
               )
             )
           )
    )
  ))
