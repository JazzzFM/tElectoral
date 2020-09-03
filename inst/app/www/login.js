$( window ).on("load",function() {
  $(".headLogin .icon").removeClass('expanded');
  var bgContainerSVG = $(".bg-container-svg");
  $(".panel-auth").append(bgContainerSVG.clone());
  bgContainerSVG.remove();
});
