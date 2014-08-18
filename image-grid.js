// how much space to leave between the images
var spacing = 5;

function alignGrid(grid) {
  var links = grid.find("a");
  var availWidth = grid.width();
  var pos = grid.offset();
  /* compute the maximal widths of the images to determine the number of columns */
  var maxWidth = spacing + Math.max.apply(Math,
                                          links.map(function(i, l) {
                                            return $(l).width();
                                          }));
  var columns = Math.floor(availWidth / maxWidth);
  // iterate over rows
  var rows = Math.ceil(links.length / columns);
  var i,j;
  var accumulatedTop = pos.top;
  for (i = 0; i < rows; i++) {
    var currentRow = $(links.slice(i * columns, (i+1) * columns));
    var maxHeight = Math.max.apply(Math, currentRow.map(function(index, l) {
      var img = $(l).children().eq(0);
      return img.outerHeight();
    }));
    // iterate over columns
    for (j = 0; j < currentRow.length; j++) {
      var link = currentRow.eq(j);
      link.css("position", "absolute");
      link.offset({
        top: accumulatedTop + Math.floor( (maxHeight - link.height()) / 2),
        left: pos.left + j * (maxWidth) + Math.floor( (maxWidth - link.width()) / 2)
      });
    }
    accumulatedTop += maxHeight + spacing;
  }
}

var processResize = false;

$(document).ready(function() {
  var $grid = $(".image-grid");
  if ($grid.length) {
    alignGrid($grid);
    $(window).resize(function() {
      if( !processResize ) {
        processResize = true;
        console.log("processing resize event");
        alignGrid($grid);
        processResize = false;
      }
    });
  }
});
