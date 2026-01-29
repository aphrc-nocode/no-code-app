# ui/sidebar_hover_collapse.R

sidebar_hover_collapse_assets <- function(
    collapsed_width = 56,   # px
    expanded_width  = 240,  # px
    transition_ms   = 320,  # smooth
    default_topbar_px = 110 # <- IMPORTANT: fallback if JS can't detect header
) {
  shiny::tags$script(shiny::HTML("
(function () {

  function killAdminLTEHeaderAndToggle() {
    // Remove the hamburger toggle wherever it exists
    document.querySelectorAll('.sidebar-toggle, a.sidebar-toggle').forEach(function(el){
      if (el && el.parentNode) el.parentNode.removeChild(el);
    });

    // Remove the AdminLTE header bar container if it exists
    var mh = document.querySelector('.main-header');
    if (mh && mh.parentNode) mh.parentNode.removeChild(mh);
  }

  // Run now
  killAdminLTEHeaderAndToggle();

  // Run again after Shiny attaches UI
  document.addEventListener('shiny:connected', function(){
    setTimeout(killAdminLTEHeaderAndToggle, 50);
    setTimeout(killAdminLTEHeaderAndToggle, 250);
    setTimeout(killAdminLTEHeaderAndToggle, 800);
  });

  // Observe DOM changes (login / dynamic UI)
  var obs = new MutationObserver(function(){ killAdminLTEHeaderAndToggle(); });
  if (document.body) obs.observe(document.body, { childList: true, subtree: true });

})();
"))
  
  css <- sprintf("
    :root{
      /* Fallback so sidebar never goes behind the green header */
      --aphrc-topbar-height: %dpx;
    }

    /* Sidebar position */
    .main-sidebar{
      position: fixed !important;
      left: 0 !important;
      top: var(--aphrc-topbar-height) !important;
      height: calc(100vh - var(--aphrc-topbar-height)) !important;

      overflow-x: hidden;
      overflow-y: auto;

      width: %dpx !important;
      transition: width %dms cubic-bezier(0.22, 1, 0.36, 1);
      will-change: width;
      z-index: 2000;
    }

    /* Fix weird top spacing inside sidebar (THIS fixes the 'home icon too high') */
    .main-sidebar .sidebar{
      padding-top: 10px !important;
    }
    .main-sidebar .sidebar-menu{
      margin-top: 0 !important;
    }

    /* Make menu items consistent height + vertically centered */
    .main-sidebar .sidebar-menu > li > a{
      height: 44px !important;
      display: flex !important;
      align-items: center !important;
      box-sizing: border-box;
    }

    /* Keep content aligned to collapsed sidebar (no jump) */
    .content-wrapper, .right-side, .main-footer{
      margin-left: %dpx !important;
      transition: margin-left %dms cubic-bezier(0.22, 1, 0.36, 1);
    }

    /* Expand on hover */
    .main-sidebar:hover{
      width: %dpx !important;
    }

    /* Hide text when collapsed */
    .main-sidebar .sidebar-menu > li > a > span,
    .main-sidebar .sidebar-menu > li > a > .pull-right-container{
      display: none !important;
    }

    /* Collapsed: icon centered */
    .main-sidebar .sidebar-menu > li > a{
      justify-content: center !important;
      padding-left: 0 !important;
      padding-right: 0 !important;
    }
    .main-sidebar .sidebar-menu > li > a > i{
      font-size: 16px;
      margin: 0 !important;
      width: auto !important;
    }

    /* On hover: show text and align left */
    .main-sidebar:hover .sidebar-menu > li > a{
      justify-content: flex-start !important;
      padding-left: 15px !important;
      padding-right: 15px !important;
    }
    .main-sidebar:hover .sidebar-menu > li > a > i{
      margin-right: 8px !important;
    }
    .main-sidebar:hover .sidebar-menu > li > a > span,
    .main-sidebar:hover .sidebar-menu > li > a > .pull-right-container{
      display: inline-block !important;
    }

    /* Keep shinydashboard header disabled (do NOT force-hide main-header aggressively) */
    .content-wrapper{ padding-top: 0 !important; }
  ",
                 default_topbar_px,
                 collapsed_width, transition_ms,
                 collapsed_width, transition_ms,
                 expanded_width
  )
  
  shiny::tagList(
    shiny::tags$style(shiny::HTML(css)),
    shiny::tags$script(shiny::HTML("
      (function () {

        function pickHeader() {
          // Try common candidates; we want the GREEN header region container
          return document.querySelector('#auth_wrapper1 .header')
              || document.querySelector('.header')
              || document.querySelector('#auth_wrapper1 header')
              || document.querySelector('header');
        }

        function setTopbarHeight() {
          var el = pickHeader();
          if (!el) return;

          // Use bottom edge: exactly where green block ends
          var r = el.getBoundingClientRect();
          var h = Math.max(0, Math.round(r.bottom));
          if (h > 20) {
            document.documentElement.style.setProperty('--aphrc-topbar-height', h + 'px');
          }
        }

        // MutationObserver: catches Shiny/login UI changes reliably
        var obs = new MutationObserver(function(){ setTopbarHeight(); });

        function startObserver() {
          if (!document.body) return;
          obs.observe(document.body, { childList: true, subtree: true });
          setTopbarHeight();
        }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', startObserver);
        } else {
          startObserver();
        }

        window.addEventListener('resize', setTopbarHeight);

      })();
    "))
  )
}
