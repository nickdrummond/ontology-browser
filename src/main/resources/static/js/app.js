import {theme} from "./theme.js";
import {minimise} from "./minimise.js";

////////////////////////////////////////////////////////////////////////////////////////////

document.addEventListener("DOMContentLoaded", function (event) {

    theme(THEME_DEFAULT, "dark", THEME_ATTRIBUTE, THEME_KEY)
        .attachTo("#darkmode");

    openFullscreen();

    burgerNavigation();

    initSearch();

    const selectorPanes = document.querySelectorAll(".side-nav");
    selectorPanes.forEach(selectorPane => minimise(selectorPane).addMinimise());
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
    const key = "burgerHidden";
    const hiddenKey = "hidden";
    const menu = document.getElementById("menu");
    const burgerHidden = localStorage.getItem(key) || false;
    if (burgerHidden) {
        menu.classList.toggle(hiddenKey);
    }
    // Add the transition after loaded to avoid animation on initial display
    setTimeout(() => {
        menu.style.transition = "height 0.3s ease-in-out";
    }, 500);

    const burger = document.getElementById("burger");
    burger.onclick = () => {
        const hiddenState = menu.classList.toggle(hiddenKey);
        localStorage.setItem(key, hiddenState);
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

