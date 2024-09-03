import {tree} from './tree.js';
import {characteristics} from './characteristics.js';
import {dlquery} from "./dlquery.js";
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

    const entityPane = entity(entityLoaded);

    if (isTree) {
        const primaryTree = document.querySelector(".owlselector.primary");
        if (primaryTree) {
            const primaryNav = tree(primaryTree, baseUrl, entityPane, rewriteLinks);

            const secondaryTree = document.querySelector(".owlselector.secondary");
            if (secondaryTree) {
                const secondaryNav = tree(secondaryTree, baseUrl, entityPane, rewriteLinks);
                secondaryNav.init();
                primaryNav.init((entityId) => {
                    secondaryNav.reload(  "secondary");
                });
            }
            else {
                primaryNav.init();
            }
        }
    }

    if (isQuery) {
        dlquery(baseUrl, entityPane).init();
    }

    entityLoaded();
});

function entityLoaded() {
    characteristics("#content").init(".characteristic, #metrics");

    if (editingEnabled) {
        edits().init(".characteristic");
    }
}

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
        cache: false,
        callback: function (obj) {
            window.location = obj.id;
        }
    });
}

