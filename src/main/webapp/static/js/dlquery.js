/*
 * Control of AJAX calls to build the query form dynamically and request results from the ontology kit
 *
 */
var BUSY_IMAGE = baseUrl + "static/images/busy.gif";

var PARAM_QUERYTYPE = "query";
var PARAM_EXPRESSION = "expression";
var PARAM_SYNTAX = "syntax";

var MAX_RETRIES = 3;

var queryURL = baseUrl + "dlquery/results";

////////////////////////////////////////////////////////////////////////////////////////////

$(document).ready(function(){
    if(document.getElementById('dlQuery')) {
        sendQuery();
    }
});

function getParameter(key) {
    return new URLSearchParams(window.location.search).get(key);
}

function sendQuery(){
    var expression = getValueOfElementByID("dlQuery");
    var query = getQueryFromForm();

    if ((expression != "") && (query != "")){
        var syntax = getValueOfElementByID("dlQuerySyntax");
        sendSubQuery(expression, syntax, query, 1);
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

function sendSubQuery(expression, syntax, queryType, retry){

    var xmlHttpReq = getXmlHttpObject();

    if (xmlHttpReq==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        var req = queryURL + "?" + PARAM_QUERYTYPE + "=" + queryType + "&" +
                             PARAM_EXPRESSION + "=" + expression;

        xmlHttpReq.open("GET", req, true);

        xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpReq.onload = function() {
            var response = xmlHttpReq.responseText;
            var status = xmlHttpReq.status;

            if (status==200) { // OK
                resultsForm.innerHTML=response;
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
                    sendSubQuery(expression, syntax, queryType, retry+1);
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
    var resultsForm = document.getElementById("resultsForm");
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