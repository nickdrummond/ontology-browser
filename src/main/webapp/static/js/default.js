

var xmlHttpOption;                         // XML HTTP object for option
var onSuccess;                             // page to go to once the option has been set (in the onload)
var baseUrl;                               // this is set by the java side
var optionsURL = "options/";
var hierarchyURL = "hierarchy/";
var HIDDEN = "hidden.";

////////////////////////////////////////////////////////////////////////////////////////////

$(document).ready(function(){

    openFullscreen();

    hideCharacteristics();

    scrollTreeToSelection();

    createSlideToggles();

    createTreeListeners();

    createAurebeshHandler();
});

function openFullscreen() {
  if (document.requestFullscreen) {
    elem.requestFullscreen();
  } else if (document.webkitRequestFullscreen) { /* Safari */
    elem.webkitRequestFullscreen();
  } else if (document.msRequestFullscreen) { /* IE11 */
    elem.msRequestFullscreen();
  }
}

function scrollTreeToSelection() {
    var minihierarchy = $(".minihierarchy");
    if (minihierarchy.is(":visible")){
        var active = $("span.active-entity", minihierarchy);
        if (active.size() > 0) {
            // let js work out getting into the pane
            active.get(0).scrollIntoView(false);
            // then reposition to the middle
            var p = minihierarchy.scrollTop();
            if (p > 0) {
                var h = minihierarchy.height();
                minihierarchy.scrollTop(p+(0.5*h));
            }
        }
    }
}

function createSlideToggles() {
    $("<img class=\"min\" src=\"" + baseUrl + "static/images/min.png\" width=\"16\" height=\"16\"/>").click(function(e){
        var values = $(this).nextAll("ul, table").first(); // for some reason just next does not work
        var hidden = values.is(":visible");
        var characteristic = $(this).next("h4").text();
        rememberCharacteristicHidden(characteristic, hidden);

        values.slideToggle('fast');
    }).prependTo(".characteristic, #owlselector, #metrics");
}

function createAurebeshHandler() {
    var key = "language";
    var aurebesh = "aurebesh";
    var basic = "basic";

    function setLanguage(lang) {
      var notLang = (lang == aurebesh) ? basic : aurebesh;
      sessionStorage.setItem(key, lang);
      $("html, #find").addClass(lang).removeClass(notLang);
      $("#aurebesh").addClass(notLang).removeClass(lang).html(notLang);
    }

    setLanguage((sessionStorage.getItem(key) == aurebesh) ? aurebesh : basic);

    $("#aurebesh").click(function(e){
        setLanguage((sessionStorage.getItem(key) == aurebesh) ? basic : aurebesh);
    });
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
        $("h4:contains('" + characteristic + "')").nextAll("ul, table").first().hide();
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

    var nodeUrl = $('a', li).first().attr('href');
    var nodeUrlPieces = nodeUrl.split('?');
    var url = nodeUrlPieces[0] + query;
    if (nodeUrlPieces[1]) {
        url = url + '?' + nodeUrlPieces[1];
    }

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