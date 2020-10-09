var hexItems = $(".hexItem");
$(window).on('load',function(){
    // InserShapestIntoHexItems(hexItems);

    // Setting first hover
    setTimeout(() => {
        $(hexItems[0]).addClass('hover');
        $(".HexModalContainer .HexModal").removeClass('active');
        var modalHexRef = $(hexItems[0]).attr('modal-hex-ref');
        $(".HexModalContainer").addClass('active');
        $(".HexModalContainer .HexModal[modal-hex-ref='"+modalHexRef+"']").addClass('active');
        $(hexItems[0]).append(`<span><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 183 183">
    <defs>
        <style>
            .cls-3 {
                fill: none;
                stroke: currentColor;
                stroke-width: 3px;
                stroke-dasharray: 150px;
                animation: drawLine .5s ease-in-out forwards;
                animation-delay: .5s;
                opacity: 0;
            }
            @keyframes drawLine{
                to{
                    opacity: 1;
                    stroke-dasharray: 100px;
                }
            }
        </style>
    </defs>
    <title>h-shape-2_1</title>
    <g id="Capa_2" data-name="Capa 2">
        <g id="Capa_1-2" data-name="Capa 1">
            <g class="cls-1">
                <path class="cls-3"
                d="M103.08,17.91C143.26,41.13,137.17,30.57,137.17,77s6.09,35.87-34.09,59.09-28,23.21-68.16,0S.83,123.43.83,77-5.26,41.13,34.92,17.91,62.91-5.3,103.08,17.91Z" />
            </g>
        </g>
    </g>
</svg></span>`);
    }, 500);
});

function InserShapestIntoHexItems(items){
    $.each(items,function(){
        var i = $(this);
        var Hex = createHexSVG(i.attr('data-title'));
        i.append(Hex);
    })
}

function createHexSVG(titulo){
    // Creating svg & polygon
    var Hex = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    var Polygon = document.createElementNS("http://www.w3.org/2000/svg", "polygon");
    var TitleLine = document.createElementNS("http://www.w3.org/2000/svg", "polyline");
    var TitleText = document.createElementNS("http://www.w3.org/2000/svg", "text");
    // Adding viewbox
    Hex.setAttribute("width", "183");
    Hex.setAttribute("height", "183");
    Hex.setAttribute("class", "bg-hexagon");
    Hex.setAttribute("ViewBox","0 0 183 183");
    Hex.setAttribute("xmlns","http://www.w3.org/2000/svg");

    // Adding settings to polygon
    Polygon.setAttribute("points", "182.84 75.52 137.27 150.54 46.15 150.54 0.58 75.52 46.15 0.5 137.27 0.5 182.84 75.52");

    // Setting title
    var PxWidth = 5;
    var TitleWidth = (titulo.length * PxWidth); // Ancho en pixeles aprox.
    TitleLine.setAttribute("points",`183 20 ${(183 - TitleWidth)} 20 ${(183 - (TitleWidth + 10) )} 32`);
    TitleLine.setAttribute("class","title-line");

    $(TitleText).text(titulo);
    TitleText.setAttribute("x", 73);
    TitleText.setAttribute("y", 15);

    // Adding to Hex
    Hex.appendChild(Polygon);
    Hex.appendChild(TitleLine);
    Hex.appendChild(TitleText);
    console.log(Hex);
    return Hex;
}

/* Click control for href panels */

$(document).on('click', ".hexItem", function(){
    var element = $(this);
    element.addClass('selectedHex');
    $(".hexSection a:not(:has(.hexItem.selectedHex)) .hexItem").addClass('hideHex');
    
    
    var b = element.attr("href");
    $(`a[href="#shiny-${b}"]`).tab('show');
    
    $(".hexItem").removeClass("selectedHex");
    $(".hexItem").removeClass("hideHex");
    return true;
})

/* End of click control */

/* Hover control */

$(".hexItem").hover(function(){
    $(".hexItem").removeClass('hover');
    $(".hexItem").find("span").remove();
    $(this).addClass('hover');
    $(this).append(`<span><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 183 183">
    <defs>
        <style>
            .cls-3 {
                fill: none;
                stroke: currentColor;
                stroke-width: 3px;
                stroke-dasharray: 150px;
                animation: drawLine .5s ease-in-out forwards;
                animation-delay: .5s;
                opacity: 0;
            }
            @keyframes drawLine{
                to{
                    opacity: 1;
                    stroke-dasharray: 100px;
                }
            }
        </style>
    </defs>
    <title>h-shape-2_1</title>
    <g id="Capa_2" data-name="Capa 2">
        <g id="Capa_1-2" data-name="Capa 1">
            <g class="cls-1">
                <path class="cls-3"
                d="M103.08,17.91C143.26,41.13,137.17,30.57,137.17,77s6.09,35.87-34.09,59.09-28,23.21-68.16,0S.83,123.43.83,77-5.26,41.13,34.92,17.91,62.91-5.3,103.08,17.91Z" />
            </g>
        </g>
    </g>
</svg></span>`);
    $(".HexModalContainer .HexModal").removeClass('active');
    var modalHexRef = $(this).attr('modal-hex-ref');
    $(".HexModalContainer").addClass('active');
    $(".HexModalContainer .HexModal[modal-hex-ref='"+modalHexRef+"']").addClass('active');
}, function(){
    // Se habilita primer elemento
    $(this).find("span").remove();
    $(hexItems[0]).addClass('hover');
    $(hexItems[0]).append(`<span><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 183 183">
    <defs>
        <style>
            .cls-3 {
                fill: none;
                stroke: currentColor;
                stroke-width: 3px;
                stroke-dasharray: 150px;
                animation: drawLine .5s ease-in-out forwards;
                animation-delay: .5s;
                opacity: 0;
            }
            @keyframes drawLine{
                to{
                    opacity: 1;
                    stroke-dasharray: 100px;
                }
            }
        </style>
    </defs>
    <title>h-shape-2_1</title>
    <g id="Capa_2" data-name="Capa 2">
        <g id="Capa_1-2" data-name="Capa 1">
            <g class="cls-1">
                <path class="cls-3"
                d="M103.08,17.91C143.26,41.13,137.17,30.57,137.17,77s6.09,35.87-34.09,59.09-28,23.21-68.16,0S.83,123.43.83,77-5.26,41.13,34.92,17.91,62.91-5.3,103.08,17.91Z" />
            </g>
        </g>
    </g>
</svg></span>`);
    $(".HexModalContainer .HexModal").removeClass('active');
    var modalHexRef = $(hexItems[0]).attr('modal-hex-ref');
    $(".HexModalContainer").addClass('active');
    $(".HexModalContainer .HexModal[modal-hex-ref='"+modalHexRef+"']").addClass('active');
    $(this).removeClass('hover');
});

/* End hover control */

/* Control de ayudas */
var ayudasContainer;
$(document).on("click", 'button', SetAyudas);
function SetAyudas(){
    if($("button.showInfo").length == 0){
     setTimeout(function() {
        ayudasContainer = $(".AyudasContainer");
        ayudasContainer.append("<button>Cerrar ayuda</button>")
        var referencias = [];
        // Asignamos ids a referencias
        $.each(ayudasContainer.find("p"), function(){
           referencias.push($(this).attr("ref-id"));
        });
        // Asignamos ids al dom
        referencias.forEach(item => {
            var element = $(`[id="${item}"]`);
            console.log(item)
            //if(!element.hasClass("form-group"))
            //    element = element.parents(".form-group");
            element.find(".control-label").prepend(`
                <button type="button" ref-id="${item}" class="showInfo"><i class="fa fa-question-circle" aria-hidden="true"></i></button>
            `);
        })
    console.log(referencias);
    }, 500);   
    }
}

$(document).on("click",".showInfo",function(){
    var element = $(this);
    ayudasContainer.addClass('show');
    ayudasContainer.find(`p`).removeClass('show');
    ayudasContainer.find(`p[ref-id="${element.attr("ref-id")}"]`).addClass('show');
})
$(document).on("click",".AyudasContainer button",function(){
    $(".AyudasContainer").removeClass("show");
    setTimeout(function() {
        $(".AyudasContainer p").removeClass("show");
    }, 1000);
})
/* End control de ayudas */