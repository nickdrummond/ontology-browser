import {BUSY_IMAGE, getXmlHttpObject, setParameter} from "./util.js";

export function loadEntity(ajaxReq, rewriteUrl, entityLoadedCallback) {

    const xmlHttpReq = getXmlHttpObject();

    if (xmlHttpReq == null) {
        alert("Browser does not support HTTP Request");
    } else {
        xmlHttpReq.open("GET", ajaxReq, true);

        xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

        xmlHttpReq.onload = function () {
            const status = xmlHttpReq.status;
            const response = xmlHttpReq.responseText;

            if (status === 200) { // OK
                const throwaway = document.createElement('span');
                throwaway.innerHTML = response;
                document.getElementById("content").replaceWith(throwaway.firstChild);

                if (rewriteUrl) {
                    window.history.pushState({}, '', rewriteUrl); // make sure URL follows
                }
                // TODO title
                entityLoadedCallback();
            } else {
                console.log(ajaxReq + ": error!", status + ":" + response);
            }
        };

        xmlHttpReq.send();

        document.getElementById("content").innerHTML = BUSY_IMAGE;
    }
}
