import {theme} from "./theme.js";
import {edits} from "./edits.js";

////////////////////////////////////////////////////////////////////////////////////////////

document.addEventListener("DOMContentLoaded", function (event) {

    theme(THEME_DEFAULT, "dark", THEME_ATTRIBUTE, THEME_KEY)
        .attachTo("#darkmode");

    if (editingEnabled) {
        edits().rememberTransaction();
    }

    openFullscreen();

    burgerNavigation();

    initSearch();

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

function burgerNavigation() {
    let burger = document.getElementById("burger");
    let menu = document.getElementById("menu");
    burger.onclick = () => {
        menu.classList.toggle("hidden");
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

