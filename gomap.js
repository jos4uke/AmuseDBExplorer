$(document).ready(function() {
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

  // reset function
  function resetGoto(){
    //alert("resetgomap clicked");
    Shiny.onInputChange("goto", {
      lat: null,
      lng: null,
      av: null,
      name: null,
      nonce: null
    });
  }
  
  // reset locator icon when reset button is clicked: not working
  document.getElementById("resetgomap").onclick=resetGoto;

});