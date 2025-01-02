debug = false
def = true

//(debug) ? console.log("js working!") : null
const z = (debug) ? console.log("js working!") : null;
// const z = (debug) ? console.log("debug = true") : null;

$(document).ready(function() {
    (debug) ? console.log("jQuery is working!") : null;
});




$(document).ready(function() {
  // Clicando no texto do switch
  $("div.seletor_ef").on("click", function() {
    (debug) ? console.log("click material-switch seletor") : null;

    // Toggle the checkbox state
    let checkbox = $("#seletor_ef");

    // Verificando o estado atual do switch
    if (checkbox.prop("checked")) {
      (debug) ? console.log("Switch is currently checked. Toggling to unchecked.") : null;
    } else {
      (debug) ? console.log("Switch is currently unchecked. Toggling to checked.") : null;
    }

    // Alterando o switch
    checkbox.prop("checked", !checkbox.prop("checked"));

    // Manualmente alterando o switch
    checkbox.trigger("change");
  });
  // Clicando no switch
  $(".material-switch").on("change", function() {
    (debug) ? console.log("seletor toggled") : null;
    if(def){
      (debug) ? console.log("def = ", def) : null;
      // Alterando cor e fonte do texto (para negrito)
      document.getElementById("left").style.color = "orange";
      document.getElementById("left").style.fontWeight = "bold";
      document.getElementById("right").style.color = "white";
      document.getElementById("right").style.fontWeight = "normal";
      def = false
      //$("#left").css("color", "white !important");
      //$("#right").css("color", "orange !important");
    }else{
      (debug) ? console.log("def = ", def) : null;
      def = true
      document.getElementById("left").style.color = "white";
      document.getElementById("left").style.fontWeight = "normal";
      document.getElementById("right").style.color = "orange";
      document.getElementById("right").style.fontWeight = "bold";
    }
  });
});
