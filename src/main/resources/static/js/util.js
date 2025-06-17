export const BUSY_IMAGE = "<img class='busy' alt='Loading...' src='" + baseUrl + "images/busy.gif" + "' />";

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

export function getSearchStrFor(url) {
    const searchParams = new URLSearchParams(url);
    return searchParams.size === 0 ? "" : "?" + searchParams.toString();
}

export function getSearchStr() {
    return getSearchStrFor(window.location.search);
}

export function getPath(url) {
    return url.split("?")[0];
}

export function getUrlWithSuffix(url, suffix) {
    return getPath(url) + suffix + getSearchStrFor(url);
}

export function setParameter(key, value) {
    window.history.pushState({}, '', getUrlWithParameter(key, value));
}

export function getUrlWithParameter(key, value) {
    let params = new URLSearchParams(window.location.search);
    params.set(key, value);
    return window.location.pathname + '?' + params;
}

export function getPlural(type) {
    switch (type) {
        case "Class": return "classes";
        case "Named individual": return "individuals";
        case "Object property": return "objectproperties";
        case "Data property": return "dataproperties";
        case "Annotation property": return "annotationproperties";
        case "Datatype": return "datatypes";
        case "ontology-uri": return "ontologies";
    }
}