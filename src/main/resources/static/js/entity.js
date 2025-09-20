import {BUSY_IMAGE, getUrlWithParameter, getUrlWithSuffix} from "./util.js";
import {characteristics} from "./characteristics.js";
import {edits} from "./edits.js";

export const entity = (sel = "#content") => {

    function loadEntity(fetchUrl, rewriteUrl) {
        fetch(fetchUrl)
            .then(response => {
                response.text().then(html => {
                    const throwaway = document.createElement('span');
                    throwaway.innerHTML = html;
                    let content = document.querySelector(sel);

                    if (rewriteUrl) {
                        window.history.pushState({}, '', rewriteUrl); // make sure URL follows
                    }

                    content.replaceWith(throwaway.firstElementChild);
                    if (response.headers.has("title")) {
                        window.document.title = response.headers.get("title");
                    }
                    entityLoaded();
                });
            })
            .catch((err) => {
                console.log(err);
                document.querySelector(sel).innerHTML = "";
            })

        document.querySelector(sel).innerHTML = BUSY_IMAGE;
    }

    function openLinksInEntityPane(parent, anchorSelector) {
        parent.onclick = (e) => {
            const link = e.target.closest(anchorSelector);
            if (link) {
                e.preventDefault();
                const type = link.getAttribute("class");
                let originalUrl = link.getAttribute("href");
                let entityId = originalUrl.split("/")[2];

                loadEntity(
                    getUrlWithSuffix(originalUrl, "/fragment"),
                    getUrlWithParameter(type.toLowerCase(), entityId) // update the URL in the link
                );
            }
        }
    }

    function entityLoaded() {
        if (editingEnabled) {
            edits().rememberTransaction();
        }

        characteristics(sel).init(".characteristic, #metrics");

        if (editingEnabled) {
            edits().init(".characteristic");
        }
    }

    if (document.querySelector(sel)?.children.length > 0) {
        entityLoaded();
    }

    return {
        loadEntity: loadEntity,
        openLinksInEntityPane: openLinksInEntityPane,
    }
}