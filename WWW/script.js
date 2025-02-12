function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}


document.querySelectorAll('input[name="theme"]').forEach((radio) => {
  radio.addEventListener("change", function () {
    document.documentElement.setAttribute("data-theme", this.value);
  });
});
