/*
 * Control of AJAX calls to build the query form dynamically and request results from the ontology kit
 *
 */
import {getXmlHttpObject, getValueOfElementByID, getParameter, setParameter, getUrlWithParameter} from "./util.js";

export const dlquery = (baseUrl, entityLoadedCallback) => {
    const BUSY_IMAGE = baseUrl + "static/images/busy.gif";

    const PARAM_QUERYTYPE = "query";
    const PARAM_EXPRESSION = "expression";
    const PARAM_MINUS = "minus";
    const PARAM_ORDER = "order";
    const PARAM_SYNTAX = "syntax";

    const MAX_RETRIES = 3;

    const queryURL = baseUrl + "dlquery/results";

////////////////////////////////////////////////////////////////////////////////////////////

    function init() {
        if (document.getElementById('dlQuery')) {
            sendQuery();
        }

        const individual = getParameter("individual");
        if (individual) {
            loadEntity("individual", "individuals", individual);
        }

        const cls = getParameter("class");
        if (cls) {
            loadEntity("class", "classes", cls);
        }
    }

    function sendQuery() {
        const expression = getValueOfElementByID("dlQuery");
        const minus = getValueOfElementByID("dlQuery2");
        const order = getValueOfElementByID("order");
        const query = getQueryFromForm();
        const start = getParameter("start");
        const pageSize = getParameter("pageSize");

        if ((expression !== "") && (query !== "")) {
            const syntax = getValueOfElementByID("dlQuerySyntax");
            sendSubQuery(expression, minus, order, syntax, query, start, pageSize, 1);
        }
    }

    function getQueryFromForm() {
        const ele = document.getElementsByName('query');
        for (let i = 0; i < ele.length; i++) {
            if (ele[i].checked) {
                return ele[i].value;
            }
        }
    }

    function sendSubQuery(expression, minus, order, syntax, queryType, start, pageSize, retry) {

        const xmlHttpReq = getXmlHttpObject();

        if (xmlHttpReq == null) {
            alert("Browser does not support HTTP Request");
        } else {
            let req = queryURL + "?" + PARAM_QUERYTYPE + "=" + queryType + "&" +
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

            xmlHttpReq.onload = function () {
                const response = xmlHttpReq.responseText;
                const status = xmlHttpReq.status;

                if (status === 200) { // OK
                    resultsForm.innerHTML = response;
                    rewriteLinks("Class", "classes"); // note Capitalised
                    rewriteLinks("individual", "individuals");
                } else {
                    resultWrite(queryType + ": error!", status + ":" + response);
                }
            };

            // timeouts
            xmlHttpReq.timeout = 10000;
            xmlHttpReq.ontimeout = function (e) {
                if (retry <= MAX_RETRIES) {
                    const t = 5 * retry; // retry at greater delay each time
                    resultWrite(queryType + ": timeout", "Slow query. Retrying in " + t + " seconds...");
                    setTimeout(function () {
                        sendSubQuery(expression, minus, order, syntax, queryType, retry + 1);
                    }, t * 1000);
                } else {
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
        resultsForm.innerHTML = "<div class='characteristic'><h4>"
            + header + "</h4><p>" + message + "</p></div>";
    }

///////////////////////

    function rewriteLinks(type, pluralType) {
        document.querySelectorAll(`#resultsForm a.${type}`).forEach(link => {
            let entityId = link.getAttribute("href").split("/")[2];
            // update the URL in the link
            link.setAttribute("href", getUrlWithParameter(type.toLowerCase(), entityId));
            // but only refresh the entity part of the page
            link.onclick = function (e) {
                e.preventDefault();
                loadEntity(type.toLowerCase(), pluralType, entityId);
            }
        });
    }

     function loadEntity(type, pluralType, entityId) {
        if (entityId == null) {
            return;
        }

        const xmlHttpReq = getXmlHttpObject();

        if (xmlHttpReq == null) {
            alert("Browser does not support HTTP Request");
        } else {
            const req = baseUrl + pluralType + "/fragment/" + entityId;

            xmlHttpReq.open("GET", req, true);

            xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

            xmlHttpReq.onload = function () {
                const status = xmlHttpReq.status;
                const response = xmlHttpReq.responseText;

                if (status === 200) { // OK
                    const responseHolder = document.createElement('span');
                    responseHolder.innerHTML = response;
                    document.getElementById("content").replaceWith(responseHolder.firstChild);
                    setParameter(type, entityId);
                    entityLoadedCallback()
                } else {
                    resultWrite(type + ": error!", status + ":" + response);
                }
            };

            xmlHttpReq.send();

            document.getElementById("content").innerHTML = "<img src='" + BUSY_IMAGE + "' width='10px' height='10px' />";
        }
    }

    return {
        init: init
    }
}