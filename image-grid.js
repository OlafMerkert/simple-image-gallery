function alignGrid(grid) {
  var links = grid.find("a");
  var availWidth = grid.width();
  var pos = grid.offset();
  var spacing = 5;
  /* compute the maximal widths of the images to determine the number of columns */
  var maxWidth = Math.max.apply(Math, links.map(function(i, l) { 
    return $(l).width(); 
  }));
  console.log(maxWidth);
  var columns = Math.floor(availWidth / maxWidth);
  // iterate over rows
  var rows = Math.ceil(links.length / columns);
  var i,j;
  for (i = 0; i < rows; i++) {
    var currentRow = $(links.slice(i * columns, (i+1) * columns));
    var maxHeight = Math.max.apply(Math, currentRow.map(function(index, l) {
      // console.log(l);
      var img = $(l).children().eq(0);
      console.log(img.outerHeight());
      return img.outerHeight();
    }));
    console.log(maxHeight);
    // iterate over columns
    for (j = 0; j < currentRow.length; j++) {
      var link = currentRow.eq(j);
      // link.css("position", "absolute");
      // link.offset({
      //   top: pos.top + i * (maxHeight + spacing),
      //   left: pos.left + j * (maxWidth + spacing)
      // });
    }
  }
}

$(document).load(function() {
  var $grid = $(".image-grid");
  // console.log($grid);
  if ($grid.length) {
    alignGrid($grid);
  }
});
