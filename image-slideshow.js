
function shrinkImage($slideshow) {
  var $img = $slideshow.find("img.img-thumbnail");
  var h = $img.height();
  var w = $img.width();
  // only shrink portrait images
  if (h > w) {
    var ref = ($slideshow.width() / 2);
    $img.height(ref);
    $img.width(ref * w / h);
  }
}

var processResize = false;

$(document).ready(function() {
  var $slideshow = $(".slideshow");
  if ($slideshow.length) {
    shrinkImage($slideshow);
    $(window).resize(function() {
      if( !processResize) {
        processResize = true;
        console.log("processing resize event for slideshow");
        shrinkImage($slideshow);
        processResize = false;
      }
    });
  }
});
