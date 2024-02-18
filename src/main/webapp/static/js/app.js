import {tree} from './tree.js';
import {characteristics} from './characteristics.js';
import {dlquery} from "./dlquery.js";
import {classToggler} from "./classToggler.js";

////////////////////////////////////////////////////////////////////////////////////////////


$(document).ready(function(){

    openFullscreen();

    classToggler("light", "dark")
        .withTargetSelector("html, #find")
        .attachTo("#darkmode");

    if (isTree) {
        tree(baseUrl, () => {
            characteristics().init("#content .characteristic, #metrics");
        }, rewriteLinks).init();
    }

    if (isQuery) {
        dlquery(baseUrl, () => {
            characteristics().init("#content .characteristic, #metrics");
        }).init();
    }

    characteristics().init(".characteristic, .owlselector, #metrics");
});

function openFullscreen() {
    if (document.requestFullscreen) {
        elem.requestFullscreen();
    } else if (document.webkitRequestFullscreen) { /* Safari */
        elem.webkitRequestFullscreen();
    } else if (document.msRequestFullscreen) { /* IE11 */
        elem.msRequestFullscreen();
    }
}

