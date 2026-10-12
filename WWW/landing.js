(function () {
  var LANG_KEY = 'nocode_lang';
  var OPEN_TAB_KEY = 'nocode_open_tab';
  var OPEN_TAB_WAIT_MS = 60000;
  var SESSION_COOKIE = /(?:^|;\s*)aphrc=[^;]+/;
  // A valid cookie is answered once the server finishes setting up the
  // dashboard, which can take a while on a cold start.
  var SESSION_WAIT_MS = 20000;

  function root() { return document.documentElement; }

  function endSessionWait() {
    root().classList.remove('lp-has-session');
  }

  function readDictionary() {
    var el = document.getElementById('lp-i18n');
    if (!el) return null;
    try { return JSON.parse(el.textContent); } catch (e) { return null; }
  }

  function applyLanguage(dict, lang) {
    var labels = dict && dict[lang];
    if (!labels) return;
    document.querySelectorAll('#landing_page [data-i18n]').forEach(function (el) {
      var value = labels[el.getAttribute('data-i18n')];
      if (value !== undefined && value !== null && value !== '') el.textContent = value;
    });
    var select = document.getElementById('lp-lang');
    if (select) select.value = lang;
    try { localStorage.setItem(LANG_KEY, lang); } catch (e) {}
  }

  // Shiny does not render outputs inside hidden elements and only rechecks
  // visibility on 'shown'/'hidden' events, so these must be triggered.
  function showAuthForm(formId) {
    ['login_form', 'signup_form', 'reset_form'].forEach(function (id) {
      var el = document.getElementById(id);
      if (!el) return;
      var show = id === formId;
      el.style.display = show ? '' : 'none';
      if (window.jQuery) jQuery(el).trigger(show ? 'shown' : 'hidden');
    });
    document.querySelectorAll('.lp-auth-tab').forEach(function (tab) {
      tab.classList.toggle('active', tab.getAttribute('data-form') === formId);
    });
  }

  // The sidebar is drawn by the server after login, and the app re-clicks the
  // active menu item when its loader lifts, so wait for both before switching.
  function openRequestedTab() {
    var tab = null;
    try { tab = sessionStorage.getItem(OPEN_TAB_KEY); } catch (e) {}
    if (!tab) return;
    var started = Date.now();
    var poll = setInterval(function () {
      var link = document.querySelector('.sidebar-menu a[data-value="' + tab + '"]');
      var loading = document.querySelector('.waiter-overlay');
      if (link && !loading) {
        clearInterval(poll);
        try { sessionStorage.removeItem(OPEN_TAB_KEY); } catch (e) {}
        setTimeout(function () { jQuery(link).trigger('click'); }, 300);
      } else if (Date.now() - started > OPEN_TAB_WAIT_MS) {
        clearInterval(poll);
      }
    }, 300);
  }

  document.addEventListener('DOMContentLoaded', function () {
    var page = document.getElementById('landing_page');
    if (!page) return;

    var dict = readDictionary();
    var stored = null;
    try { stored = localStorage.getItem(LANG_KEY); } catch (e) {}
    if (stored) applyLanguage(dict, stored);

    var select = document.getElementById('lp-lang');
    if (select) {
      select.addEventListener('change', function () { applyLanguage(dict, select.value); });
    }

    page.addEventListener('click', function (event) {
      var tabLink = event.target.closest('[data-open-tab]');
      if (tabLink) {
        try { sessionStorage.setItem(OPEN_TAB_KEY, tabLink.getAttribute('data-open-tab')); } catch (e) {}
        showAuthForm('login_form');
        return;
      }
      var authTrigger = event.target.closest('[data-form]');
      if (authTrigger) {
        showAuthForm(authTrigger.getAttribute('data-form'));
      }
    });

    var loginOutput = page.getAttribute('data-login-output');

    if (window.jQuery) {
      jQuery(document).on('shiny:value', function (event) {
        if (event.name === loginOutput && event.value === 'TRUE') openRequestedTab();
      });
    }

    if (!root().classList.contains('lp-has-session')) return;

    var started = Date.now();
    // The login package removes a cookie it cannot read, so a vanished cookie
    // means the visitor is not signed in after all.
    var poll = setInterval(function () {
      if (!SESSION_COOKIE.test(document.cookie) || Date.now() - started > SESSION_WAIT_MS) {
        clearInterval(poll);
        endSessionWait();
      }
    }, 500);

    if (window.jQuery) {
      jQuery(document).on('shiny:value', function (event) {
        if (event.name !== loginOutput || event.value !== 'TRUE') return;
        clearInterval(poll);
        setTimeout(endSessionWait, 0);
      });
    }
  });
})();
