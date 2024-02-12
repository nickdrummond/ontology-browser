import {tree} from './tree.js';
import {characteristics} from './characteristics.js';
import {dlquery} from "./dlquery.js";

////////////////////////////////////////////////////////////////////////////////////////////


$(document).ready(function(){

    openFullscreen();

    if (isTree) {
        tree(baseUrl, () => {
            characteristics().init("#content .characteristic, #metrics");
        }).init();
    }

    if (isQuery) {
        dlquery(baseUrl, () => {
            characteristics().init("#content .characteristic, #metrics");
        }).init();
    }

    characteristics().init(".characteristic, .owlselector, #metrics");

    createAurebeshHandler();

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


function createAurebeshHandler() {
    const key = "language";
    const aurebesh = "aurebesh";
    const basic = "basic";

    function setLanguage(lang) {
        const notLang = (lang == aurebesh) ? basic : aurebesh;
        sessionStorage.setItem(key, lang);
        $("html, #find").addClass(lang).removeClass(notLang);
        $("#aurebesh").addClass(notLang).removeClass(lang).html(notLang);
    }

    setLanguage((sessionStorage.getItem(key) == aurebesh) ? aurebesh : basic);

    $("#aurebesh").click(function(e){
        setLanguage((sessionStorage.getItem(key) == aurebesh) ? basic : aurebesh);
    });
}

