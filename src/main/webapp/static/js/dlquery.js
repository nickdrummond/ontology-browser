/*
 * Control of AJAX calls to build the query form dynamically and request results from the ontology kit
 *
 */
var BUSY_IMAGE = baseUrl + "static/images/busy.gif";

var EQUIVALENTS = 0;
var SUBCLASSES = 1;
var DESCENDANTS = 2;
var SUPERCLASSES = 3;
var ANCESTORS = 4;
var INSTANCES = 5; // NOT to be confused with individuals/
var QUERY_COUNT = 6;

var PARAM_QUERYTYPE = "query";
var PARAM_EXPRESSION = "expression";
var PARAM_SYNTAX = "syntax";

var NAME = 0;
var XML_OBJ = 1;
var CALLBACK = 2;

var queryURL = baseUrl + "dlquery/results";

var queryArray = [
    ["equivalents", null, inferredEquivalentsReceived],
    ["subclasses", null, inferredSubclassesReceived],
    ["descendants", null, inferredDescendantsReceived],
    ["superclasses", null, inferredSuperclassesReceived],
    ["ancestors", null, inferredAncestorsReceived],
    ["instances", null, inferredInstancesReceived]
];

////////////////////////////////////////////////////////////////////////////////////////////

$(document).ready(function(){
    if(document.getElementById('dlQuery')) {
        sendQuery();
    }
});

function sendQuery(){
    var expression = getValueOfElementByID("dlQuery");

    if (expression != ""){

        var syntax = getValueOfElementByID("dlQuerySyntax");

        document.getElementById("resultsForm").innerHTML="";

        for (var i=0; i<QUERY_COUNT; i++){
            queryArray[i][XML_OBJ]=getXmlHttpObject(queryArray[i][CALLBACK]);
            sendSubQuery(expression, syntax, queryArray[i][NAME], queryArray[i][XML_OBJ]);
        }
    }
}

function sendSubQuery(expression, syntax, querytype, xmlHttpReq){

    if (xmlHttpReq==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        var req = queryURL + "?" + PARAM_QUERYTYPE + "=" + querytype + "&" +
                             PARAM_EXPRESSION + "=" + expression
        xmlHttpReq.open("GET", req, true);

        xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpReq.send();

        var busy = document.createElement('div');
        busy.setAttribute('id', querytype);
        busy.innerHTML = "<h4 style='display: inline;'>" + querytype +
                         "</h4><img src='" + BUSY_IMAGE + "' width='18px' height='18px' />";
        document.getElementById("resultsForm").appendChild(busy);
    }
}

function receivedResults(i) {
    if (queryArray[i][XML_OBJ].readyState==4){
        var resultElement = document.getElementById(queryArray[i][NAME]);
        var response = queryArray[i][XML_OBJ].responseText;

        if (queryArray[i][XML_OBJ].status==200) { //OK
            resultElement.innerHTML=response;
        }
        else{
            resultElement.innerHTML="<h4>" + queryArray[i][NAME] + " (0)</h4>"
        }
    }
}

function inferredEquivalentsReceived() {
    receivedResults(EQUIVALENTS);
}

function inferredSubclassesReceived() {
    receivedResults(SUBCLASSES);
}

function inferredDescendantsReceived() {
    receivedResults(DESCENDANTS);
}

function inferredSuperclassesReceived() {
    receivedResults(SUPERCLASSES);
}

function inferredAncestorsReceived() {
    receivedResults(ANCESTORS);
}

function inferredInstancesReceived() {
    receivedResults(INSTANCES);
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

function getXmlHttpObject(callback) {
    var objXMLHttp=null;
    if (window.XMLHttpRequest) { // for IE7 and other standard browsers
        objXMLHttp=new XMLHttpRequest();
    }
    else if (window.ActiveXObject) { // for IE6 and earlier
        objXMLHttp=new ActiveXObject("Microsoft.XMLHTTP");
    }
    objXMLHttp.onreadystatechange=callback;
    return objXMLHttp;
}