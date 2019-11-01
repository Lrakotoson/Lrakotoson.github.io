// Fixed navbar
$(window).scroll(function(){
    if ($(window).scrollTop() >= window.innerHeight) {
        $('header').addClass('nav_fixed');
    }
    else {
        $('header').removeClass('nav_fixed');
    }
});