$(document).ready(function () {
  $("#current-time-button").click(function () {
    $.ajax("/ajax/current-time", {
      success : function (resp) {
        $("#current-time").html(resp.time);
      }
    });
  });
});
