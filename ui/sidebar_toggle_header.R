# ui/sidebar_toggle_header.R
# ------------------------------------------------------------

# ------------------------------------------------------------

sidebar_toggle_header <- function(title = "APHRC Data Anonymization Dashboard") {
  tagList(
    tags$head(
      tags$style(HTML("
        /* Top header bar */
        .app-header {
          position: sticky;
          top: 0;
          z-index: 1500;
          background: #ffffff;
          border-bottom: 1px solid #e6e6e6;
          padding: 10px 12px;
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 12px;
        }

        .app-header .header-left {
          display: flex;
          align-items: center;
          gap: 10px;
          min-width: 0;
        }

        .app-header .header-title {
          font-size: 18px;
          font-weight: 600;
          margin: 0;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          max-width: 70vw;
        }

        /* Toggle button */
        .sidebar-toggle-btn {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 42px;
          height: 38px;
          border: 1px solid #ddd;
          border-radius: 8px;
          background: #f8f8f8;
          cursor: pointer;
        }
        .sidebar-toggle-btn:hover { background: #f0f0f0; }

        /* Smooth transitions */
        #left-panel, #right-panel {
          transition: all 0.25s ease-in-out;
        }

        /* Collapsed state: hide sidebar, expand right panel */
        body.sidebar-collapsed #left-panel {
          display: none !important;
        }
        body.sidebar-collapsed #right-panel {
          width: 100% !important;
        }

        /* If right-panel is inside a bootstrap .col-sm-8, force full width */
        body.sidebar-collapsed #right-panel.col-sm-8,
        body.sidebar-collapsed #right-panel.col-md-8,
        body.sidebar-collapsed #right-panel.col-lg-8 {
          width: 100% !important;
        }
      ")),
      tags$script(HTML("
        (function() {
          function toggleSidebar() {
            document.body.classList.toggle('sidebar-collapsed');

            // If you have any JS sizing logic (like sizeRightContainers), trigger resize
            try { window.dispatchEvent(new Event('resize')); } catch(e) {}
          }

          // Make globally accessible so onclick works
          window.__toggleSidebar = toggleSidebar;
        })();
      "))
    ),
    
    # Header UI
    div(
      class = "app-header",
      div(
        class = "header-left",
        tags$button(
          type = "button",
          class = "sidebar-toggle-btn",
          title = "Toggle sidebar",
          onclick = "__toggleSidebar();",
          tags$span(style = "font-size:18px; line-height:1;", HTML("&#9776;")) # hamburger icon
        ),
        tags$h3(class = "header-title", title)
      ),
      
      # Right side of header (optional placeholder)
      div(style = "display:flex; align-items:center; gap:8px;",
          # You can add status badges/icons later if needed
          tags$span(style = "font-size:12px; color:#666;", "")
      )
    )
  )
}
