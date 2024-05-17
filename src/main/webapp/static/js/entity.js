import {BUSY_IMAGE} from "./util.js";

export function loadEntity(url, rewriteUrl, entityLoadedCallback) {
    fetch(url)
        .then(response => {
            response.text().then(html => {
                const throwaway = document.createElement('span');
                throwaway.innerHTML = html;
                let content = document.getElementById("content");
                content.replaceWith(throwaway.firstElementChild);
                entityLoadedCallback();
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
