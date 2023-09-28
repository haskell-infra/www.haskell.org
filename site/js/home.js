// Main entry point
$(function () {
  handleFeaturesCollapse();
});

function handleFeaturesCollapse() {
  $(".features .col-md-6").click(function () {
    $(this).find(".collapse").collapse("toggle");
  });
}
