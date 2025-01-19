import {BUSY_IMAGE} from "./util.js";
import {characteristics} from "./characteristics.js";

export const entity = () => {

    function loadEntity(url, rewriteUrl) {
        fetch(url)
            .then(response => {
                response.text().then(html => {
                    const throwaway = document.createElement('span');
                    throwaway.innerHTML = html;
                    let content = document.getElementById("content");
                    content.replaceWith(throwaway.firstElementChild);
                    entityLoaded();
                });

                if (rewriteUrl) {
                    window.history.pushState({}, '', rewriteUrl); // make sure URL follows
                }

                if (response.headers.has("title")) {
                    window.document.title = response.headers.get("title");
                }
            })
            .catch((err) => {
                console.log(err);
                document.getElementById("content").innerHTML = "";
            })

        document.getElementById("content").innerHTML = BUSY_IMAGE;
    }


    function entityLoaded() {
        characteristics("#content").init(".characteristic, #metrics");
    }

    return {
        loadEntity: loadEntity,
    }
}