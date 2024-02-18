import {BUSY_IMAGE, getXmlHttpObject, setParameter} from "./util.js";

export function loadEntity(type, pluralType, entityId, entityLoadedCallback, url) {
    if (entityId == null) {
        return;
    }

    const xmlHttpReq = getXmlHttpObject();

    if (xmlHttpReq == null) {
        alert("Browser does not support HTTP Request");
    } else {
        const req = baseUrl + pluralType + "/" + entityId + "/fragment";

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
                if (url) {
                    window.history.pushState({}, '', url); // make sure URL follows
                }
                entityLoadedCallback()
            } else {
                console.log(type + ": error!", status + ":" + response);
            }
        };

        xmlHttpReq.send();

        document.getElementById("content").innerHTML = BUSY_IMAGE;
    }
}
