function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}

// Capture actual sidebar menu clicks for page visit tracking
// Uses JS so it fires on the user's real click, not on input$tabs which
// gets corrupted by the menu re-render resetting to homePage
document.addEventListener('click', function(e) {
    var link = e.target.closest('.sidebar-menu a[data-value]');
    if (!link) return;
    var tab = link.getAttribute('data-value');
    if (tab && tab !== '') {
        Shiny.setInputValue('actual_page_visit', {tab: tab, ts: Date.now()}, {priority: 'event'});
    }
});
