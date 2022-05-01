

var xmlHttpOption;                         // XML HTTP object for option
var onSuccess;                             // page to go to once the option has been set (in the onload)
var baseUrl;                               // this is set by the java side
var optionsURL = "options/";
var hierarchyURL = "hierarchy/";
var HIDDEN = "hidden.";

////////////////////////////////////////////////////////////////////////////////////////////

$(document).ready(function(){

    hideCharacteristics();

    scrollTreeToSelection();

    createSlideToggles();

    createTreeListeners();

});

function scrollTreeToSelection() {
    var minihierarchy = $(".minihierarchy");
    if (minihierarchy.is(":visible")){
    var active = $("span.active-entity", minihierarchy);
        if (active.size() > 0) {
            scrollToElement(active.first(), minihierarchy);
        }
    }
}

function createSlideToggles() {
    $("<img class=\"min\" src=\"" + baseUrl + "static/images/min.png\" width=\"16\" height=\"16\"/>").click(function(e){
        var values = $(this).nextAll("ul").first(); // for some reason just next does not work
        var hidden = values.is(":visible");
        var characteristic = $(this).next("h4").text();
        rememberCharacteristicHidden(characteristic, hidden);

        values.slideToggle('fast');
    }).prependTo(".characteristic, #owlselector");

}

function rememberCharacteristicHidden(characteristic, hidden) {
    if (hidden) {
        sessionStorage.setItem(HIDDEN + characteristic, true);
    }
    else {
        sessionStorage.removeItem(HIDDEN + characteristic);
    }
}

function hideCharacteristics() {
    let keys = Object.keys(sessionStorage);
    for(let key of keys) {
      if (key.startsWith(HIDDEN)) {
        var characteristic = key.substr(HIDDEN.length);
        $("h4:contains('" + characteristic + "')").nextAll("ul").first().hide();
      }
    }
}

function createTreeListeners(){
    // add a single listener for unexpandable tree nodes
    $(".minihierarchy").click(function(e){
        var t = $(e.target).closest('span.expandable');
        handleExpand(t.parent());
    });
}

function getContentURL(){
    if (parent.frames.length > 0){
        return parent.frames["content"].location;
    }
    else{
        return ONT_SERVER;
    }
}

function GetXmlHttpObject(callback) {
    var objXMLHttp=null;
    if (window.XMLHttpRequest) {
        objXMLHttp=new XMLHttpRequest();
    }
    else if (window.ActiveXObject) {
        objXMLHttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    objXMLHttp.onreadystatechange=callback;
    return objXMLHttp;
}

function handleExpand(li){
    var children = $("ul", li);
    if (children.length > 0){
        children.slideToggle('fast');
    }
    else{
        li.append(getChildren(li));
    }
}

function getChildren(li){
    var childList = $("<ul><li><img src=\"" + baseUrl + "static/images/small_busy.gif\" width=\"10\" height=\"10\"/></li></ul>");

    var query = 'children';
    if (li.closest('.minihierarchy').hasClass('Individuals')) {
        query = 'instances';
    }

    var url = $('a', li).first().attr('href') + query;

    $.ajax({
        url: url,
        context: li,
        success: function(data, textStatus, request){
            li.replaceWith(data); // replace the li with an expanded version
        },
        error: function(request, textStatus, errorThrown){
            // get rid of the spinner and replace with an error message
            console.error(errorThrown);
            $("ul", this).html("Sorry, cannot get children - " + textStatus);
        }
    });

    return childList;
}


function scrollToElement(element, scrollParent){

    if (!scrollParent){
        scrollParent = element.offsetParent();
    }

    var pos = element.position();
    if (pos){
        var offset = pos.top;

        if (offset < 0){
            scrollParent.scrollTop(offset);
        }
        else{
            var scrollerHeight = scrollParent.height();
            var elementHeight = element.height();
            if (offset + elementHeight > scrollerHeight){
                scrollParent.scrollTop(offset + elementHeight - (scrollerHeight/2));
            }
        }
    }
}