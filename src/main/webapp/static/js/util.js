export const BUSY_IMAGE = "<img class='busy' alt='Loading...' src='" + baseUrl + "static/images/busy.gif" + "' />";

export function getXmlHttpObject() {
    let objXMLHttp = null;
    if (window.XMLHttpRequest) { // for IE7 and other standard browsers
        objXMLHttp = new XMLHttpRequest();
    } else if (window.ActiveXObject) { // for IE6 and earlier
        objXMLHttp = new ActiveXObject("Microsoft.XMLHTTP");
    }
    return objXMLHttp;
}

// just shorthand for below
export function getValueOfElementByID(id) {
    return getValueForElement(document.getElementById(id));
}

export function getValueForElement(element) {
    switch (element.type) {
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


export function getParameter(key) {
    return new URLSearchParams(window.location.search).get(key);
}

export function setParameter(key, value) {
    window.history.pushState({}, '', getUrlWithParameter(key, value));
}

export function getUrlWithParameter(key, value) {
    let params = new URLSearchParams(window.location.search);
    params.set(key, value);
    return window.location.pathname + '?' + params;
}