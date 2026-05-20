#### ---- Admin dashboard server ---- ####

admin_server <- function(USER) {

  # ── Static label outputs (reactive to language change) ───────────────────
  output$admin_lbl_title          <- renderUI({ get_rv_labels("admin_dashboard_title") })
  output$admin_lbl_total_users    <- renderUI({ h4(get_rv_labels("admin_total_users_label"),    style="margin:0;color:#555;font-size:13px;text-transform:uppercase;") })
  output$admin_lbl_countries      <- renderUI({ h4(get_rv_labels("admin_countries_label"),      style="margin:0;color:#555;font-size:13px;text-transform:uppercase;") })
  output$admin_lbl_total_visits   <- renderUI({ h4(get_rv_labels("admin_total_visits_label"),   style="margin:0;color:#555;font-size:13px;text-transform:uppercase;") })
  output$admin_lbl_active_today   <- renderUI({ h4(get_rv_labels("admin_active_today_label"),   style="margin:0;color:#555;font-size:13px;text-transform:uppercase;") })
  # Box titles styled darker for visibility on the light green header bar
  .box_title <- function(key) {
    tags$span(get_rv_labels(key), style = "color:#1a3a1a; font-weight:700;")
  }
  output$admin_lbl_funnel_title   <- renderUI({ .box_title("admin_funnel_title") })
  output$admin_lbl_users_box      <- renderUI({ .box_title("admin_users_box_title") })
  output$admin_lbl_visited_pages  <- renderUI({ .box_title("admin_visited_pages_title") })
  output$admin_lbl_daily_trend    <- renderUI({ .box_title("admin_daily_trend_title") })
  output$admin_lbl_activity_log   <- renderUI({ .box_title("admin_activity_log_title") })

  output$admin_btn_summary_download <- renderUI({
    downloadButton("admin_download_summary", get_rv_labels("admin_download_summary_btn"),
      style = "background:#23412c; border-color:#23412c; color:#fff;")
  })
  output$admin_btn_users_download <- renderUI({
    downloadButton("admin_download_users", get_rv_labels("admin_download_users_btn"),
      style = "margin-bottom:10px; background:#7bc148; border-color:#7bc148; color:#fff;")
  })
  output$admin_btn_activity_download <- renderUI({
    downloadButton("admin_download_activity", get_rv_labels("admin_download_activity_btn"),
      style = "background:#7bc148; border-color:#7bc148; color:#fff;")
  })
  output$admin_input_date_range <- renderUI({
    dateRangeInput("admin_date_range", get_rv_labels("admin_filter_date_label"),
      start = Sys.Date() - 30, end = Sys.Date())
  })
  output$admin_input_filter_user <- renderUI({
    selectInput("admin_filter_user", get_rv_labels("admin_filter_user_label"),
      choices = "All users", selected = "All users")
  })
  output$admin_input_filter_country <- renderUI({
    selectInput("admin_filter_country", get_rv_labels("admin_filter_country_label"),
      choices = "All countries", selected = "All countries")
  })
  output$admin_btn_toggle_table <- renderUI({
    actionButton("admin_toggle_table", get_rv_labels("admin_toggle_table_hide"),
      icon = icon("minus"),
      style = "background:#bde0a3;border-color:#7bc148;color:#1a3a1a;font-size:12px;padding:3px 10px;margin-bottom:8px;")
  })
  outputOptions(output, "admin_lbl_title",          suspendWhenHidden = FALSE)
  outputOptions(output, "admin_lbl_funnel_title",   suspendWhenHidden = FALSE)
  outputOptions(output, "admin_lbl_users_box",      suspendWhenHidden = FALSE)
  outputOptions(output, "admin_lbl_visited_pages",  suspendWhenHidden = FALSE)
  outputOptions(output, "admin_lbl_daily_trend",    suspendWhenHidden = FALSE)
  outputOptions(output, "admin_lbl_activity_log",   suspendWhenHidden = FALSE)
  outputOptions(output, "admin_btn_toggle_table",   suspendWhenHidden = FALSE)

  # Toggle activity table rows
  table_visible <- reactiveVal(TRUE)
  observeEvent(input$admin_toggle_table, {
    table_visible(!table_visible())
    shinyjs::toggle("admin_activity_tbl_wrap")
    updateActionButton(session, "admin_toggle_table",
      label = if (table_visible()) get_rv_labels("admin_toggle_table_hide") else get_rv_labels("admin_toggle_table_show"),
      icon  = if (table_visible()) icon("minus") else icon("plus")
    )
  }, ignoreInit = TRUE)

  # Only load admin data when user navigates TO the admin tab — not at login
  observeEvent(input$tabs, {
    req(input$tabs == "adminPanel")
    req(isTRUE(USER$logged_in))
    req(USER$username %in% c("scygu@aphrc.org", "guest565@aphrc.org"))

    # ── Load data ────────────────────────────────────────────────────────────
    con       <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    users_raw <- DBI::dbGetQuery(con, "SELECT username, first_name, last_name, country, created_date FROM users")
    # Normalise timestamps: Unix reals (> 1 billion) → readable string, others kept as-is
    activity_raw <- DBI::dbGetQuery(con, "
      SELECT username, action,
        CASE
          WHEN CAST(timestamp AS REAL) > 1000000000
            THEN strftime('%Y-%m-%d %H:%M:%S', CAST(timestamp AS INTEGER), 'unixepoch')
          ELSE CAST(timestamp AS TEXT)
        END AS timestamp
      FROM users_activity")
    DBI::dbDisconnect(con)

    activity_df <- activity_raw[order(activity_raw$timestamp, decreasing = TRUE, na.last = TRUE), ]

    # ── Process users: format date + add last seen ───────────────────────────
    last_seen <- tapply(activity_df$timestamp, activity_df$username, max)
    users_raw$last_seen <- last_seen[users_raw$username]

    users_df <- users_raw
    users_df$created_date <- tryCatch(
      format(as.POSIXct(as.numeric(users_df$created_date), origin = "1970-01-01"), "%Y-%m-%d"),
      error = function(e) users_df$created_date
    )
    users_df$last_seen <- tryCatch(
      format(as.POSIXct(users_df$last_seen, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"),
      error = function(e) users_df$last_seen
    )
    names(users_df) <- c("Email", "First Name", "Last Name", "Country", "Joined", "Last Seen")

    # ── Page visits only ─────────────────────────────────────────────────────
    page_df <- activity_df[grepl("^page:", activity_df$action), ]
    page_df$page <- gsub("^page: ", "", page_df$action)
    page_df$date <- suppressWarnings(as.Date(substr(page_df$timestamp, 1, 10)))

    # ── Summary counts ───────────────────────────────────────────────────────
    output$admin_total_users     <- renderText(nrow(users_raw))
    output$admin_total_countries <- renderText(
      length(unique(users_raw$country[!is.na(users_raw$country) & nzchar(users_raw$country)]))
    )
    output$admin_total_visits  <- renderText(nrow(page_df))
    output$admin_active_today  <- renderText(
      length(unique(page_df$username[page_df$date == Sys.Date()]))
    )

    # ── Funnel ───────────────────────────────────────────────────────────────
    funnel_steps <- list(
      list(label = get_rv_labels("admin_funnel_loaded"),   page = "sourcedata"),
      list(label = get_rv_labels("admin_funnel_feature"),  page = "featureEngineering"),
      list(label = get_rv_labels("admin_funnel_trained"),  page = "trainModel"),
      list(label = get_rv_labels("admin_funnel_deployed"), page = "validateDeployModel")
    )
    output$admin_funnel_ui <- renderUI({
      total <- nrow(users_raw)
      steps <- lapply(seq_along(funnel_steps), function(i) {
        s     <- funnel_steps[[i]]
        n     <- length(unique(page_df$username[page_df$page == s$page]))
        pct   <- if (total > 0) round(n / total * 100) else 0
        color <- if (pct >= 60) "#7bc148" else if (pct >= 30) "#bde0a3" else "#e0e0e0"
        div(style = "flex:1; text-align:center; padding:0 8px;",
          div(style = paste0(
            "background:", color, "; border-radius:4px; padding:14px 8px; margin-bottom:6px;"
          ),
            div(style = "font-size:22px; font-weight:700; color:#1a3a1a;", n),
            div(style = "font-size:11px; color:#2d5a17; text-transform:uppercase; letter-spacing:.04em;",
              paste0(pct, "% of users"))
          ),
          div(style = "font-size:12px; color:#555; font-weight:500;", s$label),
          if (i < length(funnel_steps)) {
            div(style = "position:absolute; right:-10px; top:14px; font-size:18px; color:#aaa;", "→")
          }
        )
      })
      div(style = "display:flex; align-items:flex-start; position:relative;",
        steps)
    })

    # ── Users table ──────────────────────────────────────────────────────────
    output$admin_users_table <- DT::renderDataTable({
      DT::datatable(
        users_df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # ── Most visited pages bar chart ─────────────────────────────────────────
    output$admin_visits_chart <- renderPlot({
      page_counts <- sort(table(page_df$page), decreasing = TRUE)
      page_counts <- utils::head(page_counts, 10)
      if (length(page_counts) == 0) {
        plot.new(); text(0.5, 0.5, "No visits yet", cex = 1.2, col = "#888"); return()
      }
      par(mar = c(7, 4, 1, 1))
      barplot(page_counts, col = "#7bc148", border = NA, las = 2,
              cex.names = 0.75, ylab = "Visits")
    })

    # ── Daily visits trend line chart (last 30 days only) ────────────────────
    output$admin_daily_chart <- renderPlot({
      recent <- page_df[!is.na(page_df$date) & page_df$date >= (Sys.Date() - 30), ]
      if (nrow(recent) == 0) {
        plot.new()
        text(0.5, 0.5, "No visits in the last 30 days", cex = 1.2, col = "#888")
        return()
      }
      daily <- as.data.frame(table(recent$date))
      names(daily) <- c("date", "visits")
      daily$date   <- as.Date(as.character(daily$date))
      par(mar = c(4, 4, 1, 1))
      plot(daily$date, daily$visits,
           type = "o", pch = 16, col = "#7bc148", lwd = 2,
           xlab = "", ylab = "Visits", xaxt = "n", bty = "l",
           xlim = c(Sys.Date() - 30, Sys.Date()))
      axis.Date(1, at = seq(Sys.Date() - 30, Sys.Date(), by = "5 days"),
                format = "%b %d", las = 2, cex.axis = 0.8)
      grid(nx = NA, ny = NULL, col = "#eeeeee", lty = 1)
    })

    # ── Populate filter dropdowns ─────────────────────────────────────────────
    all_users_lbl    <- "All users"
    all_countries_lbl <- "All countries"
    updateSelectInput(session, "admin_filter_user",
      choices  = c(all_users_lbl, sort(unique(activity_df$username))),
      selected = all_users_lbl)
    updateSelectInput(session, "admin_filter_country",
      choices  = c(all_countries_lbl, sort(unique(users_raw$country[nzchar(users_raw$country)]))),
      selected = all_countries_lbl)

    # ── Activity log — reactive to filters ───────────────────────────────────
    output$admin_activity_table <- DT::renderDataTable({
      df <- activity_df

      # User filter
      u <- input$admin_filter_user
      if (!is.null(u) && u != "All users" && nzchar(u)) df <- df[df$username == u, ]

      # Country filter
      ctry <- input$admin_filter_country
      if (!is.null(ctry) && ctry != "All countries" && nzchar(ctry)) {
        users_in <- users_raw$username[users_raw$country == ctry]
        df <- df[df$username %in% users_in, ]
      }

      # Date filter — substr first 10 chars gives "YYYY-MM-DD" safely
      dr <- input$admin_date_range
      if (!is.null(dr) && nrow(df) > 0) {
        parsed_dates <- suppressWarnings(as.Date(substr(df$timestamp, 1, 10)))
        df <- df[!is.na(parsed_dates) & parsed_dates >= dr[1] & parsed_dates <= dr[2], ]
      }

      # Friendly empty state
      if (nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Info = get_rv_labels("admin_no_activity_msg")),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE
        ))
      }

      # Activity table with column visibility toggle on timestamp
      DT::datatable(df,
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "Bfrtip",
          buttons    = list(
            list(
              extend  = "colvis",
              text    = "Toggle Timestamp",
              columns = 2   # 0-based: username=0, action=1, timestamp=2
            )
          )
        ),
        rownames = FALSE)
    })

    # ── Downloads ────────────────────────────────────────────────────────────
    output$admin_download_users <- downloadHandler(
      filename = function() paste0("users_", Sys.Date(), ".csv"),
      content  = function(file) utils::write.csv(users_df, file, row.names = FALSE)
    )
    output$admin_download_activity <- downloadHandler(
      filename = function() paste0("activity_", Sys.Date(), ".csv"),
      content  = function(file) {
        df <- activity_df
        dr <- input$admin_date_range
        if (!is.null(dr)) {
          parsed <- suppressWarnings(as.Date(substr(df$timestamp, 1, 10)))
          df <- df[!is.na(parsed) & parsed >= dr[1] & parsed <= dr[2], ]
        }
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # ── Summary report export ─────────────────────────────────────────────────
    output$admin_download_summary <- downloadHandler(
      filename = function() paste0("summary_report_", Sys.Date(), ".csv"),
      content  = function(file) {

        # Section 1 — key counts
        s1 <- data.frame(
          Section = "Overview",
          Metric  = c("Total Users", "Countries Represented",
                      "Total Page Visits", "Active Today"),
          Value   = c(
            nrow(users_raw),
            length(unique(users_raw$country[!is.na(users_raw$country) & nzchar(users_raw$country)])),
            nrow(page_df),
            length(unique(page_df$username[page_df$date == Sys.Date()]))
          )
        )

        # Section 2 — funnel
        s2 <- do.call(rbind, lapply(funnel_steps, function(s) {
          n   <- length(unique(page_df$username[page_df$page == s$page]))
          pct <- if (nrow(users_raw) > 0) round(n / nrow(users_raw) * 100) else 0
          data.frame(Section = "Funnel", Metric = s$label,
                     Value = paste0(n, " users (", pct, "%)"))
        }))

        # Section 3 — top pages
        pg <- sort(table(page_df$page), decreasing = TRUE)
        s3 <- data.frame(
          Section = "Top Pages",
          Metric  = names(pg),
          Value   = as.integer(pg)
        )

        # Section 4 — users by country
        ctry_tbl <- sort(table(users_raw$country[nzchar(users_raw$country)]), decreasing = TRUE)
        s4 <- data.frame(
          Section = "Users by Country",
          Metric  = names(ctry_tbl),
          Value   = as.integer(ctry_tbl)
        )

        utils::write.csv(rbind(s1, s2, s3, s4), file, row.names = FALSE)
      }
    )
  })
}
