$( window ).on("load",function() {
  $(".headLogin .icon").removeClass('expanded');
  $(".panel-auth").append(`
    <div class="bg-container-svg">
        <div></div>
        <div></div>
        <div></div>
        <div></div>
        <div></div>
    </div>
  `);
});