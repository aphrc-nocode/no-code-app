admin_ui <- function() {
  tabItem(tabName = "adminPanel",
    fluidRow(
      column(width = 9,
        h3(uiOutput("admin_lbl_title"),
           style = "color:#23412c; font-weight:700; margin-bottom:20px;")
      ),
      column(width = 3,
        div(style = "text-align:right; padding-top:16px;",
          uiOutput("admin_btn_summary_download")
        )
      )
    ),

    # ── Summary counts ──────────────────────────────────────────────────────
    fluidRow(
      column(width = 3,
        box(width = 12, status = "success", solidHeader = FALSE,
          uiOutput("admin_lbl_total_users"),
          textOutput("admin_total_users"),
          tags$style("#admin_total_users { font-size:32px; font-weight:700; color:#23412c; }")
        )
      ),
      column(width = 3,
        box(width = 12, status = "success", solidHeader = FALSE,
          uiOutput("admin_lbl_countries"),
          textOutput("admin_total_countries"),
          tags$style("#admin_total_countries { font-size:32px; font-weight:700; color:#23412c; }")
        )
      ),
      column(width = 3,
        box(width = 12, status = "success", solidHeader = FALSE,
          uiOutput("admin_lbl_total_visits"),
          textOutput("admin_total_visits"),
          tags$style("#admin_total_visits { font-size:32px; font-weight:700; color:#23412c; }")
        )
      ),
      column(width = 3,
        box(width = 12, status = "success", solidHeader = FALSE,
          uiOutput("admin_lbl_active_today"),
          textOutput("admin_active_today"),
          tags$style("#admin_active_today { font-size:32px; font-weight:700; color:#23412c; }")
        )
      )
    ),

    # ── Workflow funnel ──────────────────────────────────────────────────────
    fluidRow(
      column(width = 12,
        box(
          title = uiOutput("admin_lbl_funnel_title"), width = 12,
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          uiOutput("admin_funnel_ui")
        )
      )
    ),

    # ── Registered users table ───────────────────────────────────────────────
    fluidRow(
      column(width = 12,
        box(
          title = uiOutput("admin_lbl_users_box"), width = 12,
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          uiOutput("admin_btn_users_download"),
          DT::dataTableOutput("admin_users_table")
        )
      )
    ),

    # ── Charts ───────────────────────────────────────────────────────────────
    fluidRow(
      column(width = 6,
        box(
          title = uiOutput("admin_lbl_visited_pages"), width = 12,
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          plotOutput("admin_visits_chart", height = "300px")
        )
      ),
      column(width = 6,
        box(
          title = uiOutput("admin_lbl_daily_trend"), width = 12,
          status = "success", solidHeader = TRUE, collapsible = TRUE,
          plotOutput("admin_daily_chart", height = "300px")
        )
      )
    ),

    # ── Activity log ─────────────────────────────────────────────────────────
    fluidRow(
      column(width = 12,
        box(
          title = uiOutput("admin_lbl_activity_log"), width = 12,
          status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
          fluidRow(
            column(width = 3, uiOutput("admin_input_date_range")),
            column(width = 3, uiOutput("admin_input_filter_user")),
            column(width = 3, uiOutput("admin_input_filter_country")),
            column(width = 3,
              div(style = "margin-top:25px;", uiOutput("admin_btn_activity_download"))
            )
          ),
          br(),
          uiOutput("admin_btn_toggle_table"),
          div(id = "admin_activity_tbl_wrap",
            DT::dataTableOutput("admin_activity_table")
          )
        )
      )
    )
  )
}
