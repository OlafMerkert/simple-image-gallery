function portraitMode() {
    return $(window).height() > $(window).width();
}

function shrinkImage($slideshow) {
    var $img = $slideshow.find("img.img-thumbnail");
    var $window = $(window);
    var h = $img.height();
    var w = $img.width();
    // only shrink portrait images, so that they fit on the screen
    if (h > w) {
        var ref;
        if (portraitMode()) {
            ref = 0.9 * $window.height();
        } else {
            ref = Math.min(0.9 * $window.height(),
                           $slideshow.width()); 
        }
        console.log("resizing to height " + ref);
        $img.height(ref);
        $img.width(ref * w / h);
    } else {
        // TODO 
    }
}


function slideshowMotion(label) {
    var $motions = $(".slideshow-motion");
    var $m;
    for (var i = 0; i < $motions.length; i++) {
        if ($motions[i].text == label) {
            $m = $motions[i];
            break;
        }
    }
    location.href = $m.href;
}

function slideshowForward() {
    return slideshowMotion("Next");
}

function slideshowBackward() {
    return slideshowMotion("Previous");
}


var processResize = false;

$(document).ready(function() {
    // resizing event
    var $slideshow = $(".image-slideshow");
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
    // navigation with arrow keys
    $(document).keypress(function (event) {
        console.log("keypress code = " + event.keyCode);
        if (event.keyCode == 37) {
            // left arrow key
            slideshowBackward();
        } else if(event.keyCode == 39) {
            // right arrow key
            slideshowForward();
        }
    });
    // TODO navigation with swipes
    // $(document).on("swipeleft", slideshowBackward());
    // $(document).on("swiperight", slideshowForward());
    
});
