import {BUSY_IMAGE, getPlural} from "./util.js";
import {entity} from "./entity.js";

const ACTIVE_ENTITY = "active-entity";
const MINIHIERARCHY = '.minihierarchy';

document.addEventListener("DOMContentLoaded", function (event) {

    const entityPane = entity();

    const primaryTree = document.querySelector(".owlselector.primary");
    if (primaryTree) {
        const primaryNav = tree(primaryTree, baseUrl, entityPane);

        const secondaryTree = document.querySelector(".owlselector.secondary");
        if (secondaryTree) {
            const secondaryNav = tree(secondaryTree, baseUrl, entityPane);
            secondaryNav.init();
            primaryNav.init((entityId) => {
                secondaryNav.reload(window.location + "/secondary");
            });
        } else {
            primaryNav.init();
        }
    }

});

const tree = (treeElement, baseUrl, entityPane) => {

    let selectedEntityCallback;

    function init(callback) {
        selectedEntityCallback = callback;

        scrollToSelection();

        createExpandListeners();
    }

    function showSelected(component) {
        const active = component.querySelectorAll("." + ACTIVE_ENTITY);
        if (active.length > 0) {
            // let js work out getting into the pane
            active.item(0).scrollIntoView(false);
            // then reposition to the middle
            const p = component.scrollTop;
            if (p > 0) {
                const h = component.clientHeight;
                component.scrollTo(0, p + (0.5 * h));
            }
        }
    }

    function scrollToSelection() {
        treeElement.querySelectorAll(MINIHIERARCHY).forEach(h => {
            showSelected(h);
        });
    }

    function createExpandListeners() {
        // add a single listener for expandable tree nodes
        // we hook this on the treeElement as that doesn't change
        // but we check for events in the minihierarchy
        treeElement.onclick = (e) => {
            if (e.target.closest(MINIHIERARCHY)) {
                const t = e.target.closest('li.with-children span');
                if (t) {
                    toggleExpand(t.parentNode);
                } else {
                    const link = e.target.closest("a");
                    if (link) {
                        entitySelected(e, link);
                    }
                }
            }
        }
    }

    function toggleExpand(li) {
        li.classList.toggle('expanded');
        const children = li.querySelectorAll("ul");
        if (children.length === 0) {
            li.append(getChildren(li));
        }
    }

    function entitySelected(e, link) {
        // TODO this is a hack to allow original links to work - eg clouds
        if (TREE_LINKS_SIMPLE === true) {
            return;
        }

        e.preventDefault();

        const searchParams = new URLSearchParams(window.location.search);
        const searchStr = searchParams.size === 0 ? "" : "?" + searchParams.toString();

        const originalUrl = link.getAttribute("href");
        const entityId = getEntityId(originalUrl);
        const pluralType = getPlural(link.className);

        if (originalUrl) {
            window.history.pushState({}, '', originalUrl); // make sure URL follows
        }

        updateSelection(link);

        // TODO move url gen out
        const fragmentUrl = "/" + pluralType + "/" + entityId + "/fragment" + searchStr;
        entityPane.loadEntity(fragmentUrl, originalUrl);
        if (selectedEntityCallback) {
            selectedEntityCallback(entityId);
        }
    }

    function getChildren(li) {
        const childList = document.createElement('ul');
        childList.innerHTML = `<li>${BUSY_IMAGE}</li>`;

        const url = buildChildrenRequestUrl(li);

        fetch(url)
            .then(response => {
                response.text().then(html => {
                    const dummy = document.createElement("div");
                    dummy.innerHTML = html;
                    let expandedNode = dummy.firstElementChild;
                    childList.replaceWith(expandedNode); // replace the spinner with the children
                })
            })
            .catch(err => {
                console.error(err);
                const dummy = document.createElement("ul");
                dummy.innerHTML = "<li>???</li>";
                li.querySelector("ul").replaceWith(dummy);
            });

        return childList;
    }

    function reload(url) {
        fetch(url)
            .then(response => {
                if (!response.ok) {
                    throw new Error("(" + response.status + ") " + response.statusText);
                }
                response.text().then(html => {
                    if (html) {
                        const throwaway = document.createElement('span');
                        throwaway.innerHTML = html;
                        treeElement.replaceChildren(...throwaway.firstElementChild.childNodes);
                    } else {
                        treeElement.innerHTML = "<h4>Members (0)</h4>";
                    }
                });

                if (response.headers.has("title")) {
                    window.document.title = response.headers.get("title");
                }
            })
            .catch((err) => {
                treeElement.innerHTML = "<h4>" + err + "</h4>";
            })

        treeElement.innerHTML = BUSY_IMAGE;
    }

    function updateSelection(element) {
        // switch selection
        treeElement.querySelectorAll("." + ACTIVE_ENTITY)
            .forEach(activeEntity =>
                activeEntity.classList.remove(ACTIVE_ENTITY)
            );
        element.classList.add(ACTIVE_ENTITY);
    }

    function getStatsName(li) {
        return li.querySelector('.node').getAttribute('data');
    }

    function buildChildrenRequestUrl(li) {
        const statsName = getStatsName(li);

        // TODO there is a tidier way to construct the query
        const nodeUrl = li.querySelector('a').href;
        const [path, search] = nodeUrl.split('?');
        let url = path + '/children?';
        if (search) {
            url = url + search + "&";
        }
        if (statsName && url.indexOf("statsName=") === -1) {
            url = url + "statsName=" + statsName;
        }
        return url;
    }

    function getEntityId(originalUrl) {
        // entity ID is last path element
        // brittle - happens to work for both entity pages and relations pages
        // this depends on URLScheme staying in sync
        const pathElements = originalUrl.split("?")[0].split("/");
        const id = pathElements[pathElements.length - 1];
        return id;
    }

    return {
        init: init,
        reload: reload,
    };
};