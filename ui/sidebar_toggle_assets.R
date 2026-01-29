# ui/sidebar_toggle_assets.R

# Button to place in your existing custom header (headertag)
sidebar_toggle_button <- function() {
  tags$a(
    href = "#",
    id = "customSidebarToggle",
    role = "button",
    style = "color: white; font-size: 20px; padding: 10px 14px; display: inline-block; text-decoration: none;",
    icon("bars")
  )
}

# CSS + JS to:
# - hide the shinydashboard header
# - remove top padding offsets
# - toggle sidebar-collapse when our custom button is clicked
sidebar_toggle_assets <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        /* Hide built-in shinydashboard header bar */
        .main-header { display: none !important; }

        /* Remove offsets created by header */
        .content-wrapper, .right-side { margin-top: 0 !important; }
        .main-sidebar, .left-side { padding-top: 0 !important; }

        /* Ensure our custom toggle is visible/clickable */
        #customSidebarToggle { cursor: pointer; }
      ")),
      tags$script(HTML("
        (function() {
          // Bind once
          if (window.__customSidebarToggleBound) return;
          window.__customSidebarToggleBound = true;

          document.addEventListener('click', function(e) {
            var el = e.target.closest('#customSidebarToggle');
            if (!el) return;

            e.preventDefault();
            document.body.classList.toggle('sidebar-collapse');
            window.dispatchEvent(new Event('resize'));
          });
        })();
      "))
    )
  )
}
