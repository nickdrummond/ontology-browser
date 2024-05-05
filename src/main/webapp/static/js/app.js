import {tree} from './tree.js';
import {characteristics} from './characteristics.js';
import {dlquery} from "./dlquery.js";
import {classToggler} from "./classToggler.js";

////////////////////////////////////////////////////////////////////////////////////////////


$(document).ready(function(){

    openFullscreen();

    burgerNavigation();

    fillSearch();

    classToggler("light", "dark")
        .withTargetSelector("html, #search")
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

const NONE = "none";

function isHidden(el) {
    return (el.offsetParent === null)
}

function burgerNavigation() {
    let burger = document.getElementById("burger");
    if (!isHidden(burger)) { // setup once
        let tabs = document.getElementById("tabs");
        let defaultStyle = tabs.style.display;
        tabs.style.display = NONE; // hide by default
        burger.onclick = () => {
            if (tabs.style.display === NONE) {
                tabs.style.display = defaultStyle;
            } else {
                tabs.style.display = NONE;
            }
        }
    }
}

function fillSearch() {
    const search = new URLSearchParams(window.location.search).get('search');
    if (search) {
        let searchBox = document.getElementById('search');
        searchBox.setAttribute("value", search);
        searchBox.selectionStart = searchBox.selectionEnd = searchBox.value.length;
        searchBox.focus();
    }
}

