  // When locator icon in datatable is clicked, go to that spot on the map
  $(document).on("click", ".go-map", function(e) {
    e.preventDefault();
    $el = $(this);
    var lat = $el.data("lat");
    var long = $el.data("long");
    var av = $el.data("av");
    var name = $el.data("name")
    $($("#nav a")[0]).tab("show");
    Shiny.onInputChange("goto", {
      lat: lat,
      lng: long,
      av: av,
      name: name,
      nonce: Math.random()
    });
  });
});