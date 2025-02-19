import {theme} from "./theme.js";
import {edits} from "./edits.js";
import {entity} from "./entity.js";

////////////////////////////////////////////////////////////////////////////////////////////

document.addEventListener("DOMContentLoaded", function(event) {

    theme(THEME_DEFAULT, "dark", THEME_ATTRIBUTE, THEME_KEY)
        .attachTo("#darkmode");

    if (editingEnabled) {
        edits().rememberTransaction();
    }

    openFullscreen();

    burgerNavigation();

    initSearch();

    entity();
    if (editingEnabled) { // TODO when entity reloaded
        edits().init(".characteristic");
    }
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

function initSearch() {
    const search = new URLSearchParams(window.location.search).get('search');
    if (search) {
        let searchBox = document.getElementById('search');
        searchBox.setAttribute("value", search);
        searchBox.selectionStart = searchBox.selectionEnd = searchBox.value.length;
        searchBox.focus();
    }

    new AutoSuggest("search", {
        script: baseUrl + 'entities/?',
        varname: 'name',
        timeout: 5000,
        cache: false,
        callback: function (obj) {
            window.location = obj.id;
        }
    });
}

