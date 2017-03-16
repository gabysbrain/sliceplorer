
function showDatasetDiv(id) {
  $(".image-content .image-pane").hide();
  $(id).show();
}

$(document).ready(function() {
  // put the color codes in the reuslts table
  $("#table-section tbody td").each(function(i, a) {
    var content = a.innerHTML.trim();
    if(+content >= 0 && +content <= 3) {
      a.innerHTML = '<span class="color-code">' + content + '</span>';
    }
  });
  $(".color-code").each(function(i, a) {
    $(a).addClass("code-" + a.innerHTML.trim());
  });

  $("#table-section tbody tr").click(function() {
    if($(this).data("href"))
      window.location = $(this).data("href");
  });

  // hide/show the dataset examples
  showDatasetDiv($("#dataset-selector").val());
  $("#dataset-selector").on('change', function() {
    showDatasetDiv($(this).val());
  });

});

