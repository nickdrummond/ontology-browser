import {getValueOfElementByID, getParameter, getUrlWithParameter} from "./util.js";
import {loadEntity} from "./entity.js";
import {BUSY_IMAGE} from "./util.js";

export const dlquery = (baseUrl, entityLoadedCallback) => {

    const PARAM_QUERYTYPE = "query";
    const PARAM_EXPRESSION = "expression";
    const PARAM_MINUS = "minus";
    const PARAM_ORDER = "order";
    const PARAM_SYNTAX = "syntax";

    const MAX_RETRIES = 3;

    const queryURL = baseUrl + "dlquery/results";

////////////////////////////////////////////////////////////////////////////////////////////

    function init() {

        const options = {
            parser : baseUrl + 'dlquery/parse',
            autocomplete: baseUrl + 'dlquery/ac'
        };
        new ExpressionEditor("dlQuery", options).initialise();
        new ExpressionEditor("dlQuery2", options).initialise();

        if (document.getElementById('dlQuery')) {
            sendQuery();
        }

        const individual = getParameter("individual");
        if (individual) {
            const url = baseUrl + "individuals/" + individual + "/fragment";
            loadEntity(url, null, entityLoadedCallback);
        }

        const cls = getParameter("class");
        if (cls) {
            const url = baseUrl + "classes/" + cls + "/fragment";
            loadEntity(url, null, entityLoadedCallback);
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

        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), 10000);

        fetch(req, {
            signal: controller.signal
        }).then(response => {
            clearTimeout(timeoutId);
            response.text().then(html => {
                resultsForm.innerHTML = html;
                rewriteLinks("Class", "classes"); // note Capitalised
                rewriteLinks("individual", "individuals");
            });
        }).catch(err => {
            if (err.name === "AbortError") {
                if (retry <= MAX_RETRIES) {
                    const t = 5 * retry; // retry at greater delay each time
                    resultWrite(queryType + ": timeout", "Slow query. Retrying in " + t + " seconds...");
                    setTimeout(function () {
                        sendSubQuery(expression, minus, order, syntax, queryType, retry + 1);
                    }, t * 1000);
                } else {
                    resultWrite(queryType + ": timeout", "Perhaps your query is a little heavy for this poor server." +
                        " Please <a href='https://github.com/nickdrummond/star-wars-ontology/issues'>let us know</a>");
                }
            }
            else {
                resultWrite(queryType + ": error!", "error fetching" + ": " + err);
            }
        });

        resultWrite(queryType + BUSY_IMAGE, "");
    }

    function resultWrite(header, message) {
        const resultsForm = document.getElementById("resultsForm");
        resultsForm.innerHTML = "<div class='characteristic'><h4>"
            + header + "</h4><p>" + message + "</p></div>";
    }

///////////////////////

// TODO the links should be set correctly in the backend
    function rewriteLinks(type, pluralType) {
        document.querySelectorAll(`#resultsForm a.${type}`).forEach(link => {
            let originalUrl = link.getAttribute("href");
            let entityId = originalUrl.split("/")[2];
            // update the URL in the link
            let newUrl = getUrlWithParameter(type.toLowerCase(), entityId);
            link.setAttribute("href", newUrl);
            // but only refresh the entity part of the page
            link.onclick = function (e) {
                e.preventDefault();
                loadEntity(originalUrl + "fragment", newUrl, entityLoadedCallback);
            }
        });
    }

    return {
        init: init
    }
}