/*
 * Control of AJAX calls to build the query form dynamically and request results from the ontology kit
 *
 */
var BUSY_IMAGE = baseUrl + "static/images/busy.gif";

var PARAM_QUERYTYPE = "query";
var PARAM_EXPRESSION = "expression";
var PARAM_MINUS = "minus";
var PARAM_ORDER = "order";
var PARAM_SYNTAX = "syntax";

var MAX_RETRIES = 3;

var queryURL = baseUrl + "dlquery/results";

////////////////////////////////////////////////////////////////////////////////////////////

$(document).ready(function(){
    if(document.getElementById('dlQuery')) {
        sendQuery();
    }
    if (getParameter("individual")) {
        const individual = getParameter("individual");
        loadEntity("individuals", individual);
    }
});

function getParameter(key) {
    return new URLSearchParams(window.location.search).get(key);
}

function setParameter(key, value) {
    let params = new URLSearchParams(window.location.search);
    params.set(key, value);
    window.history.pushState({}, '', window.location.pathname + '?' + params);
}

function sendQuery(){
    var expression = getValueOfElementByID("dlQuery");
    var minus = getValueOfElementByID("dlQuery2");
    var order = getValueOfElementByID("order");
    var query = getQueryFromForm();
    var start = getParameter("start");
    var pageSize = getParameter("pageSize");

    if ((expression != "") && (query != "")){
        var syntax = getValueOfElementByID("dlQuerySyntax");
        sendSubQuery(expression, minus, order, syntax, query, start, pageSize, 1);
    }
}

function getQueryFromForm() {
    var ele = document.getElementsByName('query');
    for(i=0; i<ele.length; i++) {
        if(ele[i].checked) {
            return ele[i].value;
        }
    }
}

function sendSubQuery(expression, minus, order, syntax, queryType, start, pageSize, retry){

    var xmlHttpReq = getXmlHttpObject();

    if (xmlHttpReq==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        var req = queryURL + "?" + PARAM_QUERYTYPE + "=" + queryType + "&" +
                             PARAM_EXPRESSION + "=" + expression;

        if (minus) {
            req = req + "&" + PARAM_MINUS + "=" + minus;
        }
        if (order) {
            req = req + "&" + PARAM_ORDER + "=" + order;
        }
        if (start) {
            req = req + "&start=" + start;
        }
        if (pageSize) {
            req = req + "&pageSize=" + pageSize;
        }

        xmlHttpReq.open("GET", req, true);

        xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpReq.onload = function() {
            var response = xmlHttpReq.responseText;
            var status = xmlHttpReq.status;

            if (status==200) { // OK
                resultsForm.innerHTML = response;
                rewriteLinks("Class", "classes");
                rewriteLinks("individual", "individuals");
            }
            else{
                resultWrite(queryType + ": error!", status + ":" + response);
            }
        };

        // timeouts
        xmlHttpReq.timeout = 10000;
        xmlHttpReq.ontimeout = function(e) {
            if (retry <= MAX_RETRIES) {
                var t = 5 * retry; // retry at greater delay each time
                resultWrite(queryType + ": timeout", "Slow query. Retrying in " + t + " seconds...");
                setTimeout(function() {
                    sendSubQuery(expression, minus, order, syntax, queryType, retry+1);
                }, t*1000);
            }
            else {
                resultWrite(queryType + ": timeout", "Perhaps your query is a little heavy for this poor server." +
                 "Please <a href='https://github.com/nickdrummond/star-wars-ontology/issues'>let us know</a>");
            }
        }

        xmlHttpReq.send();

        resultWrite(queryType + "<img src='" + BUSY_IMAGE + "' width='10px' height='10px' />", "");
    }
}

function resultWrite(header, message) {
    const resultsForm = document.getElementById("resultsForm");
    resultsForm.innerHTML="<div class='characteristic'><h4>"
           + header + "</h4><p>" + message + "</p></div>";
}

///////////////////////

// just shorthand for below
function getValueOfElementByID(id){
    return getValueForElement(document.getElementById(id));
}

function getValueForElement(element){
    switch(element.type) {
        case "select-one":
            return element.options[element.selectedIndex].value;
        case "anchorNode":
            return element.getAttribute("title");
        case "text":     // dropthrough
        case "textarea": // dropthrough
        case "hidden":
            return element.value;
        default:
            alert("cannot get value from property element: " + element.type);
            return "";
    }
}

function focusComponent(id){
    document.getElementById(id).focus();
}

function rewriteLinks(type, pluralType) {
    document.querySelectorAll(`#resultsForm a.${type}`).forEach(link => {
        let entityId = link.getAttribute("href").split("/")[2];
        link.setAttribute("href", `javascript:loadEntity('${type}', '${pluralType}', '${entityId}')`);
    });
}

function loadEntity(type, pluralType, entityId){
    if (entityId == null) {
      return;
    }

    var xmlHttpReq = getXmlHttpObject();

    if (xmlHttpReq==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        var req = baseUrl + pluralType + "/fragment/" + entityId;

        xmlHttpReq.open("GET", req, true);

        xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpReq.onload = function() {
            const status = xmlHttpReq.status;
            const response = xmlHttpReq.responseText;

            if (status === 200) { // OK
                const responseHolder = document.createElement('span');
                responseHolder.innerHTML = response;
                document.getElementById("content").replaceWith(responseHolder.firstChild);
                setParameter(type, entityId);
            }
            else{
                resultWrite(type + ": error!", status + ":" + response);
            }
        };

        xmlHttpReq.send();

        document.getElementById("content").innerHTML =  "<img src='" + BUSY_IMAGE + "' width='10px' height='10px' />";
    }
}

/////////////////////////

function getXmlHttpObject() {
    var objXMLHttp=null;
    if (window.XMLHttpRequest) { // for IE7 and other standard browsers
        objXMLHttp=new XMLHttpRequest();
    }
    else if (window.ActiveXObject) { // for IE6 and earlier
        objXMLHttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    return objXMLHttp;
}