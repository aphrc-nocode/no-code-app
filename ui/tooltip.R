# ui/tooltip.R
# Central tooltip support for the NOCODE Shiny platform.
# Tooltip text is resolved from the Excel ui_labels sheet through get_rv_labels().

# Sidebar tabName -> Excel ui_labels column key
nocode_tooltip_keys <- function() {
  list(
    homePage = "tooltip_homePage",
    sourcedata = "tooltip_sourcedata",
    manageData = "tooltip_manageData",
    Overview = "tooltip_Overview",
    Explore = "tooltip_Explore",
    Transform = "tooltip_Transform",
    combineData = "tooltip_combineData",
    visualizeData = "tooltip_visualizeData",
    summarizeAutomatic = "tooltip_summarizeAutomatic",
    summarizeCustom = "tooltip_summarizeCustom",
    omopAnalysis = "tooltip_omopAnalysis",
    evidenceQuality = "tooltip_evidenceQuality",
    achilles = "tooltip_achilles",
    omop_visualizations = "tooltip_omop_visualizations",
    CohortConstructor = "tooltip_CohortConstructor",
    FeatureExtraction = "tooltip_FeatureExtraction",
    anonymization_quant = "tooltip_anonymization_quant",
    anonymization_qual = "tooltip_anonymization_qual",
    researchQuestions = "tooltip_researchQuestions",
    machineLearning = "tooltip_machineLearning",
    setupModels = "tooltip_setupModels",
    featureEngineering = "tooltip_featureEngineering",
    trainModel = "tooltip_trainModel",
    validateDeployModel = "tooltip_validateDeployModel",
    predictClassify = "tooltip_predictClassify",
    deeplearning = "tooltip_deeplearning",
    cnndeep = "tooltip_cnndeep",
    addResources = "tooltip_addResources",
    adminPanel = "tooltip_adminPanel",
    logoutID = "tooltip_logoutID"
  )
}

# Resolve tooltip keys using the app's get_rv_labels() function.
nocode_resolve_tooltips <- function(get_rv_labels, keys = nocode_tooltip_keys()) {
  resolved <- lapply(keys, function(label_key) {
    value <- tryCatch(
      as.character(get_rv_labels(label_key)),
      error = function(e) ""
    )
    
    value <- value[!is.na(value)]
    value <- paste(value, collapse = " ")
    value <- trimws(value)
    
    if (!nzchar(value)) {
      return(label_key)
    }
    
    value
  })
  
  names(resolved) <- unname(unlist(keys))
  resolved
}

# Send translated tooltip text to the browser.
# Call this once inside server.R after get_rv_labels exists.
nocode_tooltip_server <- function(session, get_rv_labels, keys = nocode_tooltip_keys()) {
  shiny::observe({
    rv <- get0("rv_lang", inherits = TRUE, ifnotfound = NULL)
    
    if (!is.null(rv)) {
      rv$selected_language
      rv$labelling_file_df
    }
    
    session$sendCustomMessage(
      "nocode-update-tooltips",
      nocode_resolve_tooltips(get_rv_labels, keys)
    )
  })
}

nocode_help_icon <- function(label_key, placement = "top") {
  shiny::tags$span(
    class = "nocode-help-icon",
    tabindex = "0",
    role = "button",
    `data-nocode-tooltip-key` = label_key,
    `data-placement` = placement,
    `aria-label` = label_key,
    shiny::icon("circle-question", lib = "font-awesome")
  )
}

nocode_label_with_help <- function(label, label_key, placement = "top") {
  shiny::tagList(
    shiny::span(label),
    nocode_help_icon(label_key, placement)
  )
}

nocode_section_title <- function(title, label_key, heading = "h4", placement = "right") {
  heading_fun <- switch(
    heading,
    h1 = shiny::h1,
    h2 = shiny::h2,
    h3 = shiny::h3,
    h4 = shiny::h4,
    h5 = shiny::h5,
    h6 = shiny::h6,
    shiny::h4
  )
  
  shiny::tags$div(
    class = "nocode-section-title",
    heading_fun(title),
    nocode_help_icon(label_key, placement)
  )
}

nocode_box_title <- function(title, label_key, placement = "right") {
  shiny::tagList(
    shiny::span(title),
    nocode_help_icon(label_key, placement)
  )
}

nocode_with_tooltip <- function(ui, label_key, placement = "top") {
  shiny::tags$span(
    class = "nocode-tooltip-wrap",
    tabindex = "0",
    `data-nocode-tooltip-key` = label_key,
    `data-placement` = placement,
    ui
  )
}

nocode_tooltip_assets <- function(keys = nocode_tooltip_keys()) {
  key_json <- jsonlite::toJSON(keys, auto_unbox = TRUE, null = "null")
  
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .nocode-section-title {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-bottom: 12px;
      }

      .nocode-section-title h1,
      .nocode-section-title h2,
      .nocode-section-title h3,
      .nocode-section-title h4,
      .nocode-section-title h5,
      .nocode-section-title h6 {
        margin: 0;
      }

      .nocode-help-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        margin-left: 6px;
        color: #4f6f52;
        cursor: help;
        font-size: 0.95em;
        vertical-align: middle;
        outline: none;
      }

      .nocode-help-icon:hover,
      .nocode-help-icon:focus {
        color: #2f4f32;
      }

      .nocode-tooltip-wrap {
        display: inline-block;
      }

      .nocode-tooltip-bubble {
        position: fixed;
        z-index: 999999;
        max-width: 420px;
        padding: 10px 12px;
        border-radius: 8px;
        background: rgba(33, 37, 41, 0.97);
        color: #ffffff;
        font-size: 12px;
        line-height: 1.45;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.22);
        pointer-events: none;
        opacity: 0;
        transform: translateY(4px);
        transition: opacity 120ms ease, transform 120ms ease;
        white-space: pre-line;
      }

      .nocode-tooltip-bubble.is-visible {
        opacity: 1;
        transform: translateY(0);
      }

      .nocode-tooltip-bubble::after {
        content: '';
        position: absolute;
        width: 8px;
        height: 8px;
        background: rgba(33, 37, 41, 0.97);
        transform: rotate(45deg);
      }

      .nocode-tooltip-bubble[data-active-placement='top']::after {
        bottom: -4px;
        left: 50%;
        margin-left: -4px;
      }

      .nocode-tooltip-bubble[data-active-placement='bottom']::after {
        top: -4px;
        left: 50%;
        margin-left: -4px;
      }

      .nocode-tooltip-bubble[data-active-placement='left']::after {
        right: -4px;
        top: 50%;
        margin-top: -4px;
      }

      .nocode-tooltip-bubble[data-active-placement='right']::after {
        left: -4px;
        top: 50%;
        margin-top: -4px;
      }

      .sidebar-menu a[data-nocode-tooltip-key] .menu-label::after,
      .main-sidebar a[data-nocode-tooltip-key] .menu-label::after {
        content: \"\\f059\";
        font-family: \"Font Awesome 6 Free\", \"FontAwesome\";
        font-weight: 900;
        margin-left: 6px;
        font-size: 0.82em;
        opacity: 0.65;
      }

      .sidebar-collapse .sidebar-menu a[data-nocode-tooltip-key] .menu-label::after,
      .sidebar-collapse .main-sidebar a[data-nocode-tooltip-key] .menu-label::after {
        display: none;
      }
    ")),
    
    shiny::tags$script(shiny::HTML(sprintf("
      window.NOCODE_TOOLTIP_KEYS = %s;
      window.NOCODE_TOOLTIPS = window.NOCODE_TOOLTIPS || {};

      (function() {
        function ensureBubble() {
          var bubble = document.getElementById('nocode-tooltip-bubble');

          if (!bubble) {
            bubble = document.createElement('div');
            bubble.id = 'nocode-tooltip-bubble';
            bubble.className = 'nocode-tooltip-bubble';
            bubble.setAttribute('role', 'tooltip');
            document.body.appendChild(bubble);
          }

          return bubble;
        }

        function getTooltipKey(el) {
          if (!el) return null;
          return el.getAttribute('data-nocode-tooltip-key');
        }

        function getText(el) {
          var key = getTooltipKey(el);

          if (!key) return null;

          return window.NOCODE_TOOLTIPS[key] || null;
        }

        function getPlacement(el) {
          return el.getAttribute('data-placement') || 'top';
        }

        function positionBubble(bubble, target) {
          var rect = target.getBoundingClientRect();
          var placement = getPlacement(target);
          var gap = 10;
          var pad = 10;

          bubble.style.top = '0px';
          bubble.style.left = '0px';

          var bubbleWidth = bubble.offsetWidth;
          var bubbleHeight = bubble.offsetHeight;

          var top;
          var left;
          var activePlacement = placement;

          if (placement === 'right') {
            top = rect.top + rect.height / 2 - bubbleHeight / 2;
            left = rect.right + gap;
          } else if (placement === 'left') {
            top = rect.top + rect.height / 2 - bubbleHeight / 2;
            left = rect.left - bubbleWidth - gap;
          } else if (placement === 'bottom') {
            top = rect.bottom + gap;
            left = rect.left + rect.width / 2 - bubbleWidth / 2;
          } else {
            top = rect.top - bubbleHeight - gap;
            left = rect.left + rect.width / 2 - bubbleWidth / 2;
          }

          if (top < pad) {
            top = rect.bottom + gap;
            activePlacement = 'bottom';
          }

          if (top + bubbleHeight > window.innerHeight - pad) {
            top = rect.top - bubbleHeight - gap;
            activePlacement = 'top';
          }

          if (left < pad) {
            left = pad;
          }

          if (left + bubbleWidth > window.innerWidth - pad) {
            left = window.innerWidth - bubbleWidth - pad;
          }

          bubble.style.top = top + 'px';
          bubble.style.left = left + 'px';
          bubble.setAttribute('data-active-placement', activePlacement);
        }

        function showTooltip(evt) {
          var target = evt.currentTarget;
          var text = getText(target);

          if (!text) return;

          var bubble = ensureBubble();

          bubble.textContent = text;
          bubble.classList.add('is-visible');

          positionBubble(bubble, target);
        }

        function hideTooltip() {
          var bubble = document.getElementById('nocode-tooltip-bubble');

          if (bubble) {
            bubble.classList.remove('is-visible');
          }
        }

        function bindTooltipElement(el) {
          if (!el) return;

          if (el.getAttribute('data-nocode-bound') === '1') {
            return;
          }

          el.setAttribute('data-nocode-bound', '1');
          el.addEventListener('mouseenter', showTooltip);
          el.addEventListener('focus', showTooltip);
          el.addEventListener('mouseleave', hideTooltip);
          el.addEventListener('blur', hideTooltip);
          el.addEventListener('click', hideTooltip);
        }

        function attachSidebarTooltips() {
          var keys = window.NOCODE_TOOLTIP_KEYS || {};

          Object.keys(keys).forEach(function(tabName) {
            var labelKey = keys[tabName];

            var selector =
              '.sidebar-menu a[data-value=\"' + tabName + '\"], ' +
              '.sidebar-menu a[href=\"#shiny-tab-' + tabName + '\"], ' +
              '.main-sidebar a[data-value=\"' + tabName + '\"], ' +
              '.main-sidebar a[href=\"#shiny-tab-' + tabName + '\"], ' +
              'a[data-value=\"' + tabName + '\"], ' +
              'a[href=\"#shiny-tab-' + tabName + '\"]';

            document.querySelectorAll(selector).forEach(function(link) {
              link.setAttribute('data-nocode-tooltip-key', labelKey);
              link.setAttribute('data-placement', 'right');

              var text = window.NOCODE_TOOLTIPS[labelKey];
              var label = (link.textContent || '').trim();

              if (text && label) {
                link.setAttribute('aria-label', label + ': ' + text);
              } else if (text) {
                link.setAttribute('aria-label', text);
              }

              bindTooltipElement(link);
            });
          });

          document.querySelectorAll('[data-nocode-tooltip-key]').forEach(bindTooltipElement);
        }

        function scheduleAttachTooltips() {
          setTimeout(attachSidebarTooltips, 50);
          setTimeout(attachSidebarTooltips, 300);
          setTimeout(attachSidebarTooltips, 1000);
        }

        function registerShinyHandler() {
          if (!window.Shiny || !Shiny.addCustomMessageHandler) {
            return;
          }

          if (window.NOCODE_TOOLTIP_HANDLER_BOUND) {
            return;
          }

          window.NOCODE_TOOLTIP_HANDLER_BOUND = true;

          Shiny.addCustomMessageHandler('nocode-update-tooltips', function(message) {
            window.NOCODE_TOOLTIPS = message || {};
            scheduleAttachTooltips();
          });
        }

        registerShinyHandler();

        document.addEventListener('DOMContentLoaded', function() {
          registerShinyHandler();
          scheduleAttachTooltips();
        });

        document.addEventListener('shiny:connected', function() {
          registerShinyHandler();
          scheduleAttachTooltips();
        });

        document.addEventListener('shiny:bound', scheduleAttachTooltips);
        document.addEventListener('shiny:value', scheduleAttachTooltips);
        document.addEventListener('shown.bs.tab', scheduleAttachTooltips);

        window.addEventListener('resize', hideTooltip);
        window.addEventListener('scroll', hideTooltip, true);

        if (window.MutationObserver) {
          var startObserver = function() {
            if (!document.body) {
              return;
            }

            var observer = new MutationObserver(function() {
              scheduleAttachTooltips();
            });

            observer.observe(document.body, {
              childList: true,
              subtree: true
            });
          };

          if (document.body) {
            startObserver();
          } else {
            document.addEventListener('DOMContentLoaded', startObserver);
          }
        }

                scheduleAttachTooltips()

        window.NOCODE_attachTooltips = attachSidebarTooltips

        function notifyShinyReady() {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue(
              'nocode_tooltips_ready',
              new Date().getTime(),
              { priority: 'event' }
            )
          }
        }

        notifyShinyReady()
        document.addEventListener('shiny:connected', notifyShinyReady)
      })()
    ", key_json)))
  )
}