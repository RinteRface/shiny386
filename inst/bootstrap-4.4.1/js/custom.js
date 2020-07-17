(function(){
  //_386.magicCursor();
  //_386.scrollLock();
  $('[data-toggle="tooltip"]').tooltip();
  $('[data-toggle="popover"]').popover();

  $(window).scroll(function() {
    var top = $(document).scrollTop();
    if(top > 50)
      $('#home > .navbar').removeClass('navbar-transparent');
    else
      $('#home > .navbar').addClass('navbar-transparent');
  });

  $("a[href='#']").click(function(e) {
    e.preventDefault();
  });

})();

$(function() {
  // handle dropdown menu in navbar: ensure that items are correctly inactivated
  // when one click in any other item, except the dropdown toggle...
  $(".navbar-nav > li a:not([data-toggle='dropdown'])").on("click", function() {
    $(".navbar-nav > li a").not(this).removeClass("active");
  });
});
