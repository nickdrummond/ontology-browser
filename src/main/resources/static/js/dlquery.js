import {getValueOfElementByID, getParameter} from "./util.js";
import {BUSY_IMAGE} from "./util.js";
import {entity} from "./entity.js";

document.addEventListener("DOMContentLoaded", function(event) {
    const entityPane = entity();
    dlquery(baseUrl, entityPane).init();
});

const dlquery = (baseUrl, entityPane) => {

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
            entityPane.loadEntity(url);
        }

        // TODO FIX DL QUERY
        const cls = getParameter("class");
        if (cls) {
            const url = baseUrl + "classes/" + cls + "/fragment";
            entityPane.loadEntity(url);
        }
    }

    function sendQuery() {
        const expression = getValueOfElementByID("dlQuery");
        const minus = getValueOfElementByID("dlQuery2");
        const order = getValueOfElementByID("order");
        const query = getValueOfElementByID("queries");
        const start = getParameter("start");
        const pageSize = getParameter("pageSize");

        if ((expression !== "") && (query !== "")) {
            const syntax = getValueOfElementByID("dlQuerySyntax");
            sendSubQuery(expression, minus, order, syntax, query, start, pageSize, 1);
        }
    }

    function sendSubQuery(expression, minus, order, syntax, queryType, start, pageSize, retry) {

        expression = expression.replaceAll(" ", "+");

        let req = queryURL + "?" + PARAM_QUERYTYPE + "=" + queryType + "&" +
            PARAM_EXPRESSION + "=" + expression;

        if (minus) {
            minus = minus.replaceAll(" ", "+");
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
                entityPane.openLinksInEntityPane(resultsForm, "li a");
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

    return {
        init: init
    }
}