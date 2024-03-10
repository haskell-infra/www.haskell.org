// Main entry point
$(function () {
  handleFeaturesCollapse();
});

function handleFeaturesCollapse() {
  // Allows the area/wrapper around feature copy to toggle collapse,
  // not just the expand link as set in the markup
  // https://getbootstrap.com/docs/4.0/components/collapse/#via-data-attributes
  $(".features .col-md-6").click(function () {
    $(this).find(".collapse").collapse("toggle");
  });

  const ELEMENT_SHOWN_EVENT = "shown.bs.collapse";
  const ELEMENT_HIDDEN_EVENT = "hidden.bs.collapse";

  $(".collapse").on(ELEMENT_HIDDEN_EVENT, function () {
    const featureSelector = $(this).context.id;
    $(`.features a[href="#${featureSelector}"]`).text("Show more");
  });

  $(".collapse").on(ELEMENT_SHOWN_EVENT, function () {
    const featureSelector = $(this).context.id;
    $(`.features a[href="#${featureSelector}"]`).text("Show less");
  });
}
