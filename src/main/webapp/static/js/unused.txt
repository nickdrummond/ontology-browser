/*
 * Control of AJAX calls to build the query form dynamically and request results from the ontology kit
 *
 */

var xmlHttpInferredEquivalents; // XML HTTP object for inferred equivalents results
var xmlHttpInferredSubclasses; // XML HTTP object for inferred subclasses results
var xmlHttpInferredDescendants; // XML HTTP object for inferred descendants results
var xmlHttpInferredSuperclasses; // XML HTTP object for inferred superclasses results
var xmlHttpInferredAncestors; // XML HTTP object for inferred ascendants results
var xmlHttpInferredInstances; // XML HTTP object for inferred instances results

var xmlHttpForm; // XML HTTP object for new form elements
var xmlHttpConstraint; // XML HTTP object for mutual constraints
var xmlHttpQuickDescription;  // XML HTTP object for quick description

var constrainElement; // temp to hold element we are constraining

var addButton;

var writeLocked = false; // DOM write lock used to make sure the results don't get interleaved on multiple threads

var ONT_SERVER;

var BUSY_IMAGE = "images/busy.gif";

var EQUIVALENTS = "equivalents";
var SUBCLASSES = "subclasses";
var DESCENDANTS = "descendants";
var SUPERCLASSES = "superclasses";
var ANCESTORS = "ancestors";
var INSTANCES = "instances"; // NOT to be confused with individuals/
//var REC_SUBCLASSES = "reciprocal-subclasses";

var PARAM_QUERYTYPE = "query";
var PARAM_EXPRESSION = "expression";
var PARAM_SYNTAX = "syntax";


////////////////////////////////////////////////////////////////////////////////////////////

function quickDescription(query, servletBase){

    ONT_SERVER = servletBase;

    xmlHttpQuickDescription=GetXmlHttpObject(quickDescriptionReceived);

    if (xmlHttpQuickDescription==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        xmlHttpQuickDescription.open("POST", ONT_SERVER + "QueryCreateForm/", true);

        xmlHttpQuickDescription.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpQuickDescription.send("type=qd&expression="+query);
    }
}

function quickDescriptionReceived(){
    if (xmlHttpQuickDescription.readyState==4 || xmlHttpQuickDescription.readyState=="complete") {
        document.innerHTML = xmlHttpQuickDescription.responseText;
        setAddVisible(true);
        sendQuery(getCurrentQuery());
    }
}

////////////////////////////////////////////////////////////////////////////////////////////

function sendQuery(expression, syntax){

    if (expression != ""){

        document.getElementById("resultsForm").innerHTML="";

        xmlHttpInferredEquivalents=GetXmlHttpObject(inferredEquivalentsReceived);
        sendSubQuery(expression, syntax, EQUIVALENTS, xmlHttpInferredEquivalents);

        xmlHttpInferredSubclasses=GetXmlHttpObject(inferredSubclassesReceived);
        sendSubQuery(expression, syntax, SUBCLASSES, xmlHttpInferredSubclasses);

        xmlHttpInferredDescendants=GetXmlHttpObject(inferredDescendantsReceived());
        sendSubQuery(expression, syntax, DESCENDANTS, xmlHttpInferredDescendants);

        xmlHttpInferredSuperclasses=GetXmlHttpObject(inferredSuperclassesReceived());
        sendSubQuery(expression, syntax, SUPERCLASSES, xmlHttpInferredSuperclasses);

        xmlHttpInferredAncestors=GetXmlHttpObject(inferredAncestorsReceived());
        sendSubQuery(expression, syntax, ANCESTORS, xmlHttpInferredAncestors);

        xmlHttpInferredInstances=GetXmlHttpObject(inferredInstancesReceived);
        sendSubQuery(expression, syntax, INSTANCES, xmlHttpInferredInstances);
    }
}

function sendSubQuery(expression, syntax, querytype, ONT_SERVER, xmlHttpReq){

    if (xmlHttpReq==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        xmlHttpReq.open("POST", ONT_SERVER+"query/",true);

        xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpReq.send(PARAM_QUERYTYPE + "=" + querytype + "&" +
                        PARAM_EXPRESSION + "=" + expression + "&" +
                        PARAM_SYNTAX + "=" + syntax + "&" +
                        "format=html-frag");

        var busy = document.createElement('div');
        busy.setAttribute('id', querytype);
        busy.innerHTML = "<h2 style='display: inline;'>" + querytype +
                         "</h2><img src='" + ONT_SERVER + BUSY_IMAGE + "' width='18px' height='18px' />";
        document.getElementById("resultsForm").appendChild(busy);
    }
}

function inferredEquivalentsReceived() {
    log("inferredEquivalentsReceived:");
    if (xmlHttpInferredEquivalents.readyState==4 || xmlHttpInferredEquivalents.readyState=="complete") {
        document.getElementById(EQUIVALENTS).innerHTML=xmlHttpInferredEquivalents.responseText;
    }
}

function inferredSubclassesReceived() {
    log("inferredSubclassesReceived:");
    if (xmlHttpInferredSubclasses.readyState==4 || xmlHttpInferredSubclasses.readyState=="complete") {
        document.getElementById(SUBCLASSES).innerHTML=xmlHttpInferredSubclasses.responseText;
    }
}

function inferredDescendantsReceived() {
    log("inferredDescendantsReceived:");
    if (xmlHttpInferredDescendants.readyState==4 || xmlHttpInferredDescendants.readyState=="complete") {
        document.getElementById(DESCENDANTS).innerHTML=xmlHttpInferredDescendants.responseText;
    }
}

function inferredSuperclassesReceived() {
    log("inferredSuperclassesReceived:");
    if (xmlHttpInferredSuperclasses.readyState==4 || xmlHttpInferredSuperclasses.readyState=="complete") {
        document.getElementById(SUPERCLASSES).innerHTML=xmlHttpInferredSuperclasses.responseText;
    }
}

function inferredAncestorsReceived() {
    log("inferredAncestorsReceived:");
    if (xmlHttpInferredAncestors.readyState==4 || xmlHttpInferredAncestors.readyState=="complete") {
        document.getElementById(ANCESTORS).innerHTML=xmlHttpInferredAncestors.responseText;
    }
}

function inferredInstancesReceived() {
    log("inferredInstancesReceived");
    if (xmlHttpInferredInstances.readyState==4 || xmlHttpInferredInstances.readyState=="complete") {
        document.getElementById(INSTANCES).innerHTML=xmlHttpInferredInstances.responseText;
    }
}


//function getCurrentQuery(){
//
//    var queryForm = getQueryForm();
//
//    var baseElement = document.getElementById("submissionName");
//
//    var startpoint = 1;
//
//    // get the base type from a dropdown or an autocompleter
//    var str;
//    switch (baseElement.type){
//        case "select-one":
//            str = baseElement.options[baseElement.selectedIndex].value;
//            break;
//        case "hidden":
//            str = baseElement.value;
//            startpoint = 2; // skip the textbox when getting the rest of the query form
//            break;
//        default:
//            break;
//    }
//
//    var propertyElement, typeElement, valueElement;
//
//    var elementCount = queryForm.childNodes.length;
//
//    alert("element count: " + elementCount);
//
//    for(var i=0; i<=elementCount; i++) {
//        propertyElement = queryForm.property;
//        typeElement = queryForm.getElementsByName("type")[0];
//        valueElement = queryForm.getElementsByName("value")[0];
//
//        var selectedProp = getValueForElement(propertyElement);
//        var selectedType = getValueForElement(typeElement);
//        var selectedValue = getValueForElement(valueElement);
//
//        if ((selectedProp != "") & (selectedType != "") & (selectedValue != "")){
//            str += " and (" + selectedProp + " " + selectedType + " " + selectedValue + ")";
//        }
//    }
//
//    return str;
//}

function getCurrentQuery(){

    var queryForm = getQueryForm();

    var baseElement = queryForm.elements[0];

    var str = getValueForElement(baseElement);
//    var startpoint = 1;
//
//    // get the base type from a dropdown or an autocompleter
//    var str;
//    switch (baseElement.type){
//        case "select-one":
//            str = baseElement.options[baseElement.selectedIndex].value;
//            break;
//        case "text":
//            str = baseElement.value;
//            startpoint = 2; // skip the textbox when getting the rest of the query form
//            break;
//        default:
//            alert("can't find a root");
//            break;
//    }

    var property, type, value;

    for(var i=1; i<queryForm.elements.length; i=i+3) {
        property = queryForm.elements[i];
        type = queryForm.elements[i+1];
        value = queryForm.elements[i+2];

        var selectedProp = getValueForElement(property);
        var selectedType = getValueForElement(type);
        var selectedValue = getValueForElement(value);

        if ((selectedProp != "") & (selectedType != "") & (selectedValue != "")){
            str += " and (" + selectedProp + " " + selectedType + " " + selectedValue + ")";
        }
    }

    return str;
}


////////////////////////////////////////////////////////////////////////////////////////////

function requestNewQueryElement(query, ONT_SERVER){

    xmlHttpForm=GetXmlHttpObject(queryFormElementReceived);

    if (xmlHttpForm==null) {
        alert ("Browser does not support HTTP Request");
    }
    else{
        xmlHttpForm.open("POST", ONT_SERVER+"QueryExtend/", true);

        xmlHttpForm.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpForm.send(PARAM_QUERYTYPE + "=" + query);
    }
}

function queryFormElementReceived() {
    if (xmlHttpForm.readyState==4 || xmlHttpForm.readyState=="complete") {

        var response = xmlHttpForm.responseText;

//        alert(response);

        // the content
        var propSelector = document.createElement('span');

        propSelector.innerHTML="<br />" + response;

        getQueryForm().appendChild(propSelector);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////

// gets called each time a property (or type = "some, all etc") dropdown is changed
// the callingObject is the dropdown that has been changed
function requestFillerConstraints(callingObject) {

    // this is the span that encapsulates the whole conjunct
    var conjunctElement = callingObject.parentNode;

    // get the selected filler
    var fillerElement = getNamedSubElement(conjunctElement, "value");
    if (fillerElement != null){

        // save the filler element to repopulate when our kit returns
        constrainElement = fillerElement;

        var selectedFiller = getValueForElement(constrainElement);//constrainElement.options[constrainElement.selectedIndex].value;

        // get the selected property
        var propertyElement = getNamedSubElement(conjunctElement, "property");
        var selectedProperty = getValueForElement(propertyElement);//propertyElement.options[propertyElement.selectedIndex].value;

        // get the selected type
        var typeElement = getNamedSubElement(conjunctElement, "type");
        var selectedType = getValueForElement(typeElement);//typeSel.options[typeSel.selectedIndex].value;

        xmlHttpConstraint=GetXmlHttpObject(constrainFillerResponseReceived);

        if (xmlHttpConstraint==null) {
            alert ("Browser does not support HTTP Request");
        }
        else{
            var constrainParams = "expression=" + getCurrentQuery() +
                                  "&property=" + selectedProperty +
                                  "&type=" + selectedType +
                                  "&filler=" + selectedFiller +
                                  "&syntax=qd";

//            alert(constrainParams);

            xmlHttpConstraint.open("POST", ONT_SERVER+"QueryConstrain/" ,true);

            xmlHttpConstraint.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

            xmlHttpConstraint.send(constrainParams);
        }
    }
    else{
        alert("no value element for " + conjunctElement + ", " + fillerElement);
    }
}

function getNamedSubElement(parent, name) {
    var matchingElements = document.getElementsByName(name);
    for (var i=0; i<matchingElements.length; i++){
        if (matchingElements[i].parentNode==parent){
            return matchingElements[i];
        }
    }
    return null;
}


// when the property has been changed, request that the appropriate filler values be set to the range
function constrainFillerResponseReceived() {
    if (xmlHttpConstraint.readyState==4 || xmlHttpConstraint.readyState=="complete") {
        //constrainElement.options.length=0;

        log("constrain filler response");

        // @@TODO save the selected item
        var oldValue = constrainElement.options[constrainElement.selectedIndex].value;

        // the new html is sent with the reply (this should really be constructed on the client)
        constrainElement.innerHTML=xmlHttpConstraint.responseText;

        sendQuery(getCurrentQuery());
    }
}

///////  interface handling  /////////////////////////////////////////////////////////////////////////////////////

function createAddButton(size, ONT_SERVER){
    var addB = document.createElement('img');
    addB.setAttribute('name', "addButton");
    addB.setAttribute('class', "button");
    addB.setAttribute('src', ONT_SERVER+"images/add.png");
    addB.setAttribute('width', size);
    addB.setAttribute('height', size);
    addB.setAttribute('onmouseup', "requestNewQueryElement(getCurrentQuery(document)); setAddVisible(false, document);");
    return addB;
}

//function createRemoveButton(size){
//    var removeButton = document.createElement('img');
//    removeButton.setAttribute('class', 'button');
//    removeButton.setAttribute('src', "images/remove.png");
//    removeButton.setAttribute('width', size);
//    removeButton.setAttribute('height', size);
//    removeButton.setAttribute('onmouseup', "removeFromForm(this.parentNode); setAddVisible(true);");
//    return removeButton;
//}

function removeFromForm(element) {
    var queryForm = getQueryForm();
    var query = getCurrentQuery();
    queryForm.removeChild(element);
    sendQuery(query);
}

function setAddVisible(visible, ONT_SERVER) {
    var queryForm = getQueryForm();
    if (visible && addButton == null){
        addButton = createAddButton("24", ONT_SERVER);
        queryForm.appendChild(addButton);
    }
    else if (!visible && addButton != null){
        queryForm.removeChild(addButton);
        addButton = null;
    }
}

function getQueryForm() {
    var queryform = document.getElementById("chooser");
    if (queryform == null){
        alert("null query form");
    }
    return queryform;
}

function submitenter(myfield,e)
{
    var keycode;
    if (window.event) keycode = window.event.keyCode;
    else if (e) keycode = e.which;
    else return true;

    if (keycode == 13)
    {
        myfield.form.submit();
        return false;
    }
    else
        return true;
}

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
            alert("type of property element: " + element.type);
            return "";
    }
}

function focusComponent(id){
    document.getElementById(id).focus();
}

////////////////////////////////////////////////////////////////////////////////////////////


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

function log(str){
    var header = parent.header;
    if (header != null){
        var logWindow = parent.header.document.getElementById('log');
        if (logWindow != null){
            var safestr = str.replace(/</g, "&lt;");
            safestr = safestr.replace(/>/g, "&gt;");
            logWindow.innerHTML = logWindow.innerHTML + "\n" + safestr;
        }
    }
}