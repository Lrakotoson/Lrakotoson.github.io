'use strict';
$(window).on("load", function() {

    //  //PRELOADER
     $('#preloader').delay(350).fadeOut('slow'); // will fade out the white DIV that covers the website.
    
}); // window load end

$(document).on("ready", function() {

// ONE PAGE SCROLL
if ($('.home').length) {
    $(document).on("scroll", onScroll);
    //smoothscroll
    $('.backtop, .home-down, .menu li a[href^="#"]').on('click', function (e) {
        e.preventDefault();
        $(document).off("scroll");
        $('.menu li a').each(function () {
            $(this).removeClass('selected');
            if ($(window).width() < 768) {
                $('.menu').slideUp();
            }
        })
        $(this).addClass('selected');
        var target = this.hash
            , //  menu = target;
            $target = $(target);
        $('html, body').stop().animate({
            'scrollTop': $target.offset().top - 70
        }, 500, 'swing', function () {
            window.location.hash = target;
            $(document).on("scroll", onScroll);
        });
    });

    function onScroll(event) {
        var scrollPos = $(document).scrollTop();
        $('.menu li a').each(function () {
            var currLink = $(this);
            var refElement = $(currLink.attr("href"));
            if (refElement.position().top - 73 <= scrollPos && refElement.position().top + refElement.height() > scrollPos) {
                $('.menu li a').removeClass("selected");
                currLink.addClass("selected");
            }
            else {
                currLink.removeClass("selected");
            }
        });
    }
}


//NAVBAR SHOW - HIDE
if ($('.home').length) {
    $(window).scroll(function () {
        var scroll = $(window).scrollTop();
        var homeheight = $(".home").height() - 73;
        if (scroll > homeheight) {
            $("header").addClass("nav-fixed");
        }
        else {
            $("header").removeClass("nav-fixed");
        }
    });
}


// HOME PAGE HEIGHT
if ($('.home').length) {
    function centerInit() {
        var hometext = $('.home')
        hometext.css({
            "height": $(window).height() + "px"
        });
    }
    centerInit();
    $(window).resize(centerInit);
}


// HOME TYPED JS
if ($('.element').length) {
    $('.element').each(function () {
        $(this).typed({
            strings: [$(this).data('text1'), $(this).data('text2'), $(this).data('text3')],
            loop: $(this).data('loop') ? $(this).data('loop') : false ,
            backDelay: $(this).data('backdelay') ? $(this).data('backdelay') : 2000 ,
            typeSpeed: 10,
        });
    });
}


}); // document ready end
    