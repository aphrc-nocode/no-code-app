# ui/sidebar_hover_collapse.R

sidebar_hover_collapse_assets <- function(
    collapsed_width   = 56,   # px
    expanded_width    = 240,  # px
    transition_ms     = 320,  # smooth
    default_topbar_px = 110,  # fallback if JS can't detect header
    bottom_gap_px     = 28,   # space below the pill (prevents reaching footer)
    top_gap_px        = 12    # space above the pill
) {
  
  # -----------------------------
  # JS: remove AdminLTE header + hamburger;
  #     inject a toggle item INSIDE the sidebar menu (before Home)
  # -----------------------------
  # ---- Build toggle labels from the platform translation file (same format as the app) ----
  .toggle_i18n <- list(
    English = list(
      sidebar_toggle_collapse_menu = "Collapse menu",
      sidebar_toggle_expand_menu   = "Expand menu",
      sidebar_toggle_menu_aria     = "Toggle menu"
    ),
    French = list(
      sidebar_toggle_collapse_menu = "Réduire le menu",
      sidebar_toggle_expand_menu   = "Développer le menu",
      sidebar_toggle_menu_aria     = "Basculer le menu"
    ),
    Swahili = list(
      sidebar_toggle_collapse_menu = "Kunja menyu",
      sidebar_toggle_expand_menu   = "Panua menyu",
      sidebar_toggle_menu_aria     = "Geuza menyu"
    )
  )
  
  # If the app already loaded labelling_file (data.frame) in GlobalEnv, prefer it
  if (exists("labelling_file", envir = .GlobalEnv)) {
    lf <- get("labelling_file", envir = .GlobalEnv)
    if (is.data.frame(lf) && "input_language" %in% names(lf)) {
      needed <- c(
        "input_language",
        "sidebar_toggle_collapse_menu",
        "sidebar_toggle_expand_menu",
        "sidebar_toggle_menu_aria"
      )
      if (all(needed %in% names(lf))) {
        for (lang in c("English", "French", "Swahili")) {
          row <- lf[lf$input_language == lang, , drop = FALSE]
          if (nrow(row) == 1) {
            .toggle_i18n[[lang]] <- list(
              sidebar_toggle_collapse_menu = as.character(row$sidebar_toggle_collapse_menu),
              sidebar_toggle_expand_menu   = as.character(row$sidebar_toggle_expand_menu),
              sidebar_toggle_menu_aria     = as.character(row$sidebar_toggle_menu_aria)
            )
          }
        }
      }
    }
  }
  
  js_main <- sprintf("
(function () {

  // Embedded translations from the SAME Excel-driven system (3 keys only)
  var APHRC_TOGGLE_I18N = %s;

  function killAdminLTEHeaderAndToggle() {
    document.querySelectorAll('.sidebar-toggle, a.sidebar-toggle').forEach(function(el){
      if (el && el.parentNode) el.parentNode.removeChild(el);
    });
    var mh = document.querySelector('.main-header');
    if (mh && mh.parentNode) mh.parentNode.removeChild(mh);
  }

  function pickHeader() {
    return document.querySelector('#auth_wrapper1 .header')
        || document.querySelector('.custom-header')
        || document.querySelector('.header')
        || document.querySelector('#auth_wrapper1 header')
        || document.querySelector('header');
  }

  function setTopbarHeight() {
    var el = pickHeader();
    if (!el) return;
    var r = el.getBoundingClientRect();
    var h = Math.max(0, Math.round(r.bottom));
    if (h > 20) {
      document.documentElement.style.setProperty('--aphrc-topbar-height', h + 'px');
    }
  }

  // ----------------------------
  // Language (match platform)
  // ----------------------------
  function getCurrentLangName() {
    // 1) Preferred: the real selectInput id used by your platform
    var sel = document.getElementById('change_language');
    if (sel && sel.value) return sel.value;

    // 2) Shiny input value (works even if select not found)
    try {
      if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues) {
        var v = Shiny.shinyapp.$inputValues['change_language'];
        if (v) return v;
      }
    } catch(e){}

    // 3) Fallback
    return 'English';
  }

  function t_ui(key) {
    var lang = getCurrentLangName();
    if (APHRC_TOGGLE_I18N[lang] && APHRC_TOGGLE_I18N[lang][key]) return APHRC_TOGGLE_I18N[lang][key];
    if (APHRC_TOGGLE_I18N.English && APHRC_TOGGLE_I18N.English[key]) return APHRC_TOGGLE_I18N.English[key];
    return key;
  }

  // ----------------------------
  // Sidebar toggle behavior
  // ----------------------------
  function isCollapsed() {
    return document.body.classList.contains('aphrc-sidebar-collapsed');
  }

  function syncToggleUI() {
    var item = document.getElementById('aphrcSidebarToggleItem');
    if (!item) return;

    var icon = item.querySelector('i');
    var span = item.querySelector('span');
    var a    = item.querySelector('a');
    if (!icon || !span || !a) return;

    if (isCollapsed()) {
      icon.className = 'fa-solid fa-bars';
      span.textContent = t_ui('sidebar_toggle_expand_menu');
    } else {
      icon.className = 'fa-solid fa-xmark';
      span.textContent = t_ui('sidebar_toggle_collapse_menu');
    }

    a.setAttribute('aria-label', t_ui('sidebar_toggle_menu_aria'));
    a.setAttribute('title',      t_ui('sidebar_toggle_menu_aria'));
  }

  function toggleSidebar() {
    document.body.classList.toggle('aphrc-sidebar-collapsed');
    try { localStorage.setItem('aphrcSidebarCollapsed', isCollapsed() ? '1' : '0'); } catch(e){}
    syncToggleUI();
  }

  function restoreState() {
    // Expanded by default
    try {
      var saved = localStorage.getItem('aphrcSidebarCollapsed');
      if (saved === '1') document.body.classList.add('aphrc-sidebar-collapsed');
    } catch(e){}
    syncToggleUI();
  }

  function ensureSidebarToggleItem() {
    var menu = document.querySelector('.main-sidebar .sidebar-menu');
    if (!menu) return;

    if (document.getElementById('aphrcSidebarToggleItem')) {
      syncToggleUI();
      return;
    }

    var li = document.createElement('li');
    li.id = 'aphrcSidebarToggleItem';
    li.className = 'aphrc-toggle-item';

    var a = document.createElement('a');
    a.href = 'javascript:void(0)';
    a.setAttribute('role', 'button');

    var i = document.createElement('i');
    i.className = 'fa-solid fa-xmark';

    var span = document.createElement('span');
    span.textContent = t_ui('sidebar_toggle_collapse_menu');

    a.appendChild(i);
    a.appendChild(span);

    a.addEventListener('click', function(e){
      e.preventDefault();
      toggleSidebar();
    });

    li.appendChild(a);
    menu.insertBefore(li, menu.firstChild);

    restoreState();
  }

  function watchLanguageChanges() {
    var sel = document.getElementById('change_language');
    if (!sel) return;
    sel.addEventListener('change', function(){
      setTimeout(syncToggleUI, 20);
      setTimeout(syncToggleUI, 120);
      setTimeout(syncToggleUI, 300);
    });
  }

  // Run now
  killAdminLTEHeaderAndToggle();

  // Observe DOM changes
  var obs = new MutationObserver(function(){
    killAdminLTEHeaderAndToggle();
    setTopbarHeight();
    ensureSidebarToggleItem();
  });

  function startObserver() {
    if (!document.body) return;
    obs.observe(document.body, { childList: true, subtree: true });
    setTopbarHeight();
    ensureSidebarToggleItem();
    watchLanguageChanges();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', startObserver);
  } else {
    startObserver();
  }

  document.addEventListener('shiny:connected', function(){
    setTimeout(killAdminLTEHeaderAndToggle, 50);
    setTimeout(killAdminLTEHeaderAndToggle, 250);
    setTimeout(killAdminLTEHeaderAndToggle, 800);

    setTimeout(setTopbarHeight, 80);
    setTimeout(setTopbarHeight, 350);

    setTimeout(ensureSidebarToggleItem, 120);
    setTimeout(ensureSidebarToggleItem, 400);

    setTimeout(syncToggleUI, 200);
    setTimeout(syncToggleUI, 600);
  });

  window.addEventListener('resize', function(){
    setTopbarHeight();
    syncToggleUI();
  });

})();
",
  jsonlite::toJSON(.toggle_i18n, auto_unbox = TRUE)
  )
  
  
  
  # -----------------------------
  # CSS: expanded by default; collapse ONLY by body class
  # + FIX: remove ALL hover-based label behavior
  # + Pill: oval capsule + reasonable height (leave bottom gap)
  # -----------------------------
  css <- sprintf("
:root{
  --aphrc-topbar-height: %dpx;

  --aphrc-sidebar-collapsed: %dpx;
  --aphrc-sidebar-expanded: %dpx;

  --aphrc-sidebar-ease: cubic-bezier(0.22, 1, 0.36, 1);
  --aphrc-sidebar-ms: %dms;

  /* pill spacing */
  --aphrc-pill-top-gap: %dpx;
  --aphrc-pill-bottom-gap: %dpx;
}

/* Sidebar frame */
.main-sidebar{
  position: fixed !important;
  left: 0 !important;
  top: var(--aphrc-topbar-height) !important;
  height: calc(100vh - var(--aphrc-topbar-height)) !important;

  width: var(--aphrc-sidebar-expanded) !important; /* expanded by default */
  transition: width var(--aphrc-sidebar-ms) var(--aphrc-sidebar-ease) !important;
  will-change: width;

  overflow: visible !important;
  z-index: 2000 !important;

  /* IMPORTANT: your big CSS sets pointer-events:none on .main-sidebar;
     we do NOT do that here because it breaks interactions in some layouts.
     If your other CSS still sets it, make sure .sidebar-menu has pointer-events:auto. */
  pointer-events: auto !important;
}

/* Collapsed only when user chooses */
body.aphrc-sidebar-collapsed .main-sidebar{
  width: var(--aphrc-sidebar-collapsed) !important;
}

/* Content offsets */
.content-wrapper, .right-side, .main-footer{
  margin-left: var(--aphrc-sidebar-expanded) !important;
  transition: margin-left var(--aphrc-sidebar-ms) var(--aphrc-sidebar-ease) !important;
}
body.aphrc-sidebar-collapsed .content-wrapper,
body.aphrc-sidebar-collapsed .right-side,
body.aphrc-sidebar-collapsed .main-footer{
  margin-left: var(--aphrc-sidebar-collapsed) !important;
}

/* Fix top spacing inside sidebar */
.main-sidebar .sidebar{ padding-top: 10px !important; }
.main-sidebar .sidebar-menu{ margin-top: 0 !important; }

/* Menu item baseline */
.main-sidebar .sidebar-menu > li > a{
  height: 44px !important;
  display: flex !important;
  align-items: center !important;
  box-sizing: border-box !important;
}

/* SHOW labels always when expanded */
body:not(.aphrc-sidebar-collapsed) .main-sidebar .sidebar-menu > li > a > span,
body:not(.aphrc-sidebar-collapsed) .main-sidebar .sidebar-menu > li > a > .pull-right-container{
  display: inline-block !important;
}

/* HIDE labels only when collapsed */
body.aphrc-sidebar-collapsed .main-sidebar .sidebar-menu > li > a > span,
body.aphrc-sidebar-collapsed .main-sidebar .sidebar-menu > li > a > .pull-right-container{
  display: none !important;
}

/* Alignments */
body:not(.aphrc-sidebar-collapsed) .main-sidebar .sidebar-menu > li > a{
  justify-content: flex-start !important;
  padding-left: 15px !important;
  padding-right: 15px !important;
}
body:not(.aphrc-sidebar-collapsed) .main-sidebar .sidebar-menu > li > a > i{
  margin-right: 10px !important;
}

body.aphrc-sidebar-collapsed .main-sidebar .sidebar-menu > li > a{
  justify-content: center !important;
  padding-left: 0 !important;
  padding-right: 0 !important;
}
body.aphrc-sidebar-collapsed .main-sidebar .sidebar-menu > li > a > i{
  margin: 0 !important;
  width: auto !important;
}

/* ---------------------------
   CRITICAL FIX:
   Nuke hover-based label rules from your big CSS
   --------------------------- */
.main-sidebar:hover .sidebar-menu > li > a > span,
.main-sidebar:hover .sidebar-menu > li > a > .pull-right-container{
  /* Force the correct state */
  display: inherit !important;
}
body.aphrc-sidebar-collapsed .main-sidebar:hover .sidebar-menu > li > a > span,
body.aphrc-sidebar-collapsed .main-sidebar:hover .sidebar-menu > li > a > .pull-right-container{
  display: none !important;
}

/* ---------------------------
   Toggle item styling (first item in pill)
   --------------------------- */
#aphrcSidebarToggleItem > a{
  opacity: 0.95 !important;
}
#aphrcSidebarToggleItem > a:hover{
  opacity: 1 !important;
}

/* Update label text via CSS depending on state (optional nicety) */
body.aphrc-sidebar-collapsed #aphrcSidebarToggleItem > a > span{
  display: none !important; /* collapsed = icon only, clean */
}

/* ---------------------------
   Pill shape + reasonable height
   - Oval capsule: huge radius
   - Do NOT reach footer: leave bottom gap
   --------------------------- */
.main-sidebar .sidebar-menu{
  /* keep your pill bg/shadow from main CSS; we only control geometry here */
  border-radius: 999px !important; /* perfect capsule/oval corners */

  margin-top: var(--aphrc-pill-top-gap) !important;
  margin-bottom: var(--aphrc-pill-bottom-gap) !important;

  /* Reasonable height: not to the bottom */
  max-height: calc(100vh - var(--aphrc-topbar-height) - var(--aphrc-pill-top-gap) - var(--aphrc-pill-bottom-gap)) !important;
  overflow-y: auto !important;
  overflow-x: hidden !important;
}
",
default_topbar_px,
collapsed_width,
expanded_width,
transition_ms,
top_gap_px,
bottom_gap_px
  )
  
  shiny::tagList(
    shiny::tags$style(shiny::HTML(css)),
    shiny::tags$script(shiny::HTML(js_main))
  )
}
