

$("#dataset-selector").on('change', function() {
  console.log($(this).val());
  var showDiv = $(this).val();
  $(".image-content .image-pane").hide();
  $(showDiv).show();
});


