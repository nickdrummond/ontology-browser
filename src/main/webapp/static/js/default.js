

var xmlHttpOption;                         // XML HTTP object for option
var onSuccess;                             // page to go to once the option has been set (in the onload)
var baseUrl;                               // this is set by the java side
var optionsURL = "options/";
var hierarchyURL = "hierarchy/";
var HIDDEN = "hidden.";

////////////////////////////////////////////////////////////////////////////////////////////

$(document).ready(function(){


if(typeof(Storage) !== 'undefined') {
    if (sessionStorage.showMenu === 'false') {
        hideMenu();
    }
} else {
    alert('Sorry! No Web Storage support..');
}

    hideCharacteristics();

    scrollTreeToSelection();

    createLabelRendererListener();

    createSlideToggles();

    createTreeListeners();

    createActiveOntListeners();
    createOptionListeners();
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

function createLabelRendererListener() {
    // add a listener to the render labels checkbox
    $("#renderLabels").click(function(e){
        var rendererName = "frag";
        if (this.checked){
            rendererName = "label";
        }
        option("renderer", rendererName, null);
    });
}

function hideMenu() {
    $('#menu').hide();
}

function toggleMenu(e) {
    sessionStorage.showMenu = (sessionStorage.showMenu === 'false') ? 'true' : 'false';
    $('#menu').slideToggle('fast');
    return false;
}

function createSlideToggles() {
    $("<img class=\"min\" src=\"" + baseUrl + "static/images/min.png\" width=\"16\" height=\"16\"/>").click(function(e){
        var values = $(this).nextAll("ul").first(); // for some reason just next does not work
        var hidden = values.is(":visible");
        var characteristic = $(this).next("h4").text();
        rememberCharacteristicHidden(characteristic, hidden);

        values.slideToggle('fast');
    }).prependTo(".characteristic, #owlselector");

    $('<a class="burger" href="">&equiv;</a>').click(toggleMenu).prependTo('#title');
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

function createActiveOntListeners(){
    $("#activeOnt select").change(function(e){
        xmlHttpOption=GetXmlHttpObject(function(e){
            if (xmlHttpOption.readyState==4 || xmlHttpOption.readyState=="complete") {
                //reloadAllFrames();
                console.log(e.srcElement.response);
            }
        });

        if (xmlHttpOption==null) {
            alert ("Browser does not support HTTP Request");
        }
        else{
            var url = baseUrl + "ontologies/active";
            xmlHttpOption.open("POST", url, true);
            xmlHttpOption.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");
            xmlHttpOption.send("id=" + $(this).val());
        }
    });
}

function createOptionListeners(){
    $("select.option").change(function(e){
        var value = $(this).val();
        var optionId = $("input[name=property]", $(this).parent()).attr("value");

        option(optionId, value, null);
    });

    $("input.option[type='checkbox']").click(function(e){
        var optionId = $("input[name=property]", $(this).parent()).attr("value");
        var state = "false";
        if (this.checked){
            state = "true";
        }
        option(optionId, state, null);
    });
}


// successPage is optional
// - if specified, this page will be loaded when the option is set successfully
// - if omitted, the current page will be refreshed when the option is set successfully
function option(opt, value, successpage){

    xmlHttpOption=GetXmlHttpObject(optionSet);

    if (xmlHttpOption==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        xmlHttpOption.open("POST", baseUrl + optionsURL, true);

        xmlHttpOption.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        var attrs =  "property=" + opt + "&value=" + value;

        onSuccess = successpage;

        xmlHttpOption.send(attrs);
    }
}

function optionSet(){
    if (xmlHttpOption.readyState==4 || xmlHttpOption.readyState=="complete") {
        if (onSuccess != null){
            if (parent != null){
                parent.window.location = onSuccess;
            }
            else{
                window.location = onSuccess;
            }
        }
        else{
            reloadAllFrames();
        }
    }
}

function reloadAllFrames(){
    if (parent.frames.length > 0){
        for(var i=0; i<parent.frames.length; i++){
            var sURL = parent.frames[i].location;
            parent.frames[i].location.replace(sURL);
        }
    }
    else{
        // we have to strip out the session if it is in the url
        // otherwise the previous state will just be reloaded
        var url = location.toString();
        url = url.replace(/[&|?]label=[^&]+/, "");
        location.replace(url);
    }
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