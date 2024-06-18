import {loadEntity} from "./entity.js";
import {BUSY_IMAGE} from "./util.js";

const ACTIVE_ENTITY = "active-entity";
const MINIHIERARCHY = '.minihierarchy';

export const tree = (baseUrl, entityLoadedCallback, isRewriteLinks) => {

    function init() {
        scrollTreeToSelection();
        const primaryTree = document.querySelector(".owlselector.primary");
        const secondaryTree = document.querySelector(".owlselector.secondary");
        if (secondaryTree) {
            if (primaryTree) {
                rewrite(primaryTree);
            }
            if (isRewriteLinks) {
                rewriteLinks(secondaryTree);
            }
            createTreeListeners(primaryTree, rewrite);
            createTreeListeners(secondaryTree, rewriteLinks);
        } else if (primaryTree) {
            if (isRewriteLinks) {
                rewriteLinks(primaryTree);
            }
            createTreeListeners(primaryTree, rewriteLinks);
        }
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

    function scrollTreeToSelection() {
        const h = document.querySelectorAll(MINIHIERARCHY);
        h.forEach(hierarchy => {
            showSelected(hierarchy);
        });
    }

    function createTreeListeners(parent, rewriteFunction) {
        // add a single listener for expandable tree nodes
        parent.onclick = (e) => {
            const t = e.target.closest('span.expandable');
            if (t) {
                handleExpand(t.parentNode, rewriteFunction);
            }
        };
    }

    function handleExpand(li, rewriteFunction) {
        const children = li.querySelectorAll("ul");
        if (children.length > 0) {
            $(children).slideToggle('fast'); // TODO remove JQuery slideToggle - see https://codepen.io/jorgemaiden/pen/YgGZMg
        } else {
            li.append(getChildren(li, rewriteFunction));
        }
    }

    function getChildren(li, rewriteFunction) {
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
                    if (rewriteFunction) {
                        rewriteFunction(expandedNode);
                    }
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

    // TODO work out type
    function rewriteLinks(parentElement) {
        rewriteLinksFor("Class", "classes", parentElement);
        rewriteLinksFor("Named", "individuals", parentElement);
        rewriteLinksFor("Object", "objectproperties", parentElement);
        rewriteLinksFor("Data", "dataproperties", parentElement);
        rewriteLinksFor("Annotation", "annotationproperties", parentElement);
        rewriteLinksFor("Datatype", "datatypes", parentElement);
        rewriteLinksFor("ontology-uri", "ontologies", parentElement);
    }

    // TODO a single listener for all links - with closest - but already have createTreeListeners
    function rewriteLinksFor(type, pluralType, parent) {
        parent.querySelectorAll(`a.${type}`).forEach(link => {
            let originalUrl = link.getAttribute("href");
            const entityId = getEntityId(originalUrl);

            // but only refresh the entity part of the page
            link.onclick = function (e) {
                e.preventDefault();
                selectEntity(link);
                loadEntity("/" + pluralType + "/" + entityId + "/fragment", originalUrl, entityLoadedCallback);
            }
        });
    }

    function rewrite(parent) {
        parent.querySelectorAll(`a.Class`).forEach(link => {
            let originalUrl = link.getAttribute("href");
            const entityId = getEntityId(originalUrl);

            // but only refresh the secondary hierarchy
            link.onclick = function (e) {
                e.preventDefault();
                selectEntity(link);
                loadSecondary(entityId, originalUrl);
                loadEntity("/classes/" + entityId + "/fragment", originalUrl, entityLoadedCallback);
            }
        });
    }

    function loadSecondary(entityId, rewriteUrl) {
        let content = document.getElementsByClassName("secondary").item(0);
        fetch("/individuals/by/type/" + entityId + "/fragment/direct")
            .then(response => {
                if (!response.ok) {
                    throw new Error("(" + response.status + ") " + response.statusText);
                }
                response.text().then(html => {
                    if (html) {
                        const throwaway = document.createElement('span');
                        throwaway.innerHTML = html;
                        rewriteLinks(throwaway.firstElementChild);
                        content.replaceWith(throwaway.firstElementChild);
                    } else {
                        content.innerHTML = "<h4>Members (0)</h4>";
                    }
                });

                if (rewriteUrl) {
                    window.history.pushState({}, '', rewriteUrl); // make sure URL follows
                }

                if (response.headers.has("title")) {
                    window.document.title = response.headers.get("title");
                }
            })
            .catch((err) => {
                content.innerHTML = "<h4>" + err +"</h4>";
            })

        content.innerHTML = BUSY_IMAGE;
    }

    function selectEntity(element) {
        // switch selection
        const component = element.closest(".owlselector");
        component.querySelectorAll("." + ACTIVE_ENTITY)
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
        const nodeUrlPieces = nodeUrl.split('?');
        let url = nodeUrlPieces[0] + 'children?';
        if (nodeUrlPieces[1]) {
            url = url + nodeUrlPieces[1] + "&";
        }
        if (statsName) {
            url = url + "statsName=" + statsName;
        }
        return url;
    }

    function getEntityId(originalUrl) {
        // entity ID is last path element
        // brittle - happens to work for both entity pages and relations pages
        // this depends on URLScheme staying in sync
        let pathElements = originalUrl.split("/");
        return pathElements[pathElements.length - 2];
    }

    return {
        init: init
    };
};