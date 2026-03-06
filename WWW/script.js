function get_id(clicked_id) {
  Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}

document.querySelectorAll('input[name="theme"]').forEach((radio) => {
  radio.addEventListener("change", function () {
    document.documentElement.setAttribute("data-theme", this.value);
  });
});

(function () {
  function isCollapsed() {
    return document.body.classList.contains("sidebar-collapse");
  }

  function getToggleEl() {
    return document.getElementById("sidebar_toggle_menu");
  }

  // This is the real AdminLTE toggle button used by shinydashboard
  function getAdminToggleBtn() {
    return document.querySelector(".main-header .sidebar-toggle");
  }

  function updateToggleUI() {
    var el = getToggleEl();
    if (!el) return false;

    var expandLbl = el.getAttribute("data-label-expand") || "Expand Menu";
    var collapseLbl = el.getAttribute("data-label-collapse") || "Collapse Menu";
    var collapsed = isCollapsed();

    // label
    var label = el.querySelector(".aphrc-toggle-label");
    if (label) label.textContent = collapsed ? expandLbl : collapseLbl;

    // tooltip when collapsed only
    if (collapsed) el.setAttribute("title", expandLbl);
    else el.removeAttribute("title");

    el.setAttribute("aria-label", collapsed ? expandLbl : collapseLbl);
    el.setAttribute("aria-expanded", String(!collapsed));

    return true;
  }

  function doAdminToggle() {
  var btn = getAdminToggleBtn();
  if (btn) {
    btn.click(); // AdminLTE toggle
  } else {
    document.body.classList.toggle("sidebar-collapse");
    document.body.classList.remove("sidebar-open");
    window.dispatchEvent(new Event("resize"));
  }

  // run update multiple times to catch AdminLTE class change timing
  setTimeout(updateToggleUI, 0);
  setTimeout(updateToggleUI, 50);
  setTimeout(updateToggleUI, 150);
}
// Ensure tooltip appears on hover any time it's collapsed
document.addEventListener("mouseenter", function (e) {
  var a = e.target.closest && e.target.closest("#sidebar_toggle_menu");
  if (!a) return;
  updateToggleUI();
}, true);

  // Expanded by default + wait until renderMenu creates the element
  function bootstrapWhenReady() {
    document.body.classList.remove("sidebar-collapse");

    var tries = 0;
    var timer = setInterval(function () {
      tries++;
      if (updateToggleUI()) clearInterval(timer);
      if (tries >= 80) clearInterval(timer);
    }, 100);
  }

  // click on our custom toggle row
  document.addEventListener("click", function (e) {
    var a = e.target.closest && e.target.closest("#sidebar_toggle_menu");
    if (!a) return;

    e.preventDefault();
    e.stopPropagation();

    doAdminToggle();
  });

  document.addEventListener("DOMContentLoaded", bootstrapWhenReady);
  document.addEventListener("shiny:connected", bootstrapWhenReady);
  window.addEventListener("load", bootstrapWhenReady);
})();