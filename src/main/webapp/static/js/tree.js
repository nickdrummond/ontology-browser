import {BUSY_IMAGE} from "./util.js";

const ACTIVE_ENTITY = "active-entity";
const MINIHIERARCHY = '.minihierarchy';

export const tree = (treeElement, baseUrl, entityPane, isRewriteLinks) => {

    let selectedEntityCallback;

    function init(callback) {
        selectedEntityCallback = callback;

        scrollToSelection();
        if (isRewriteLinks) {
            rewriteLinks(treeElement);
        }
        createExpandListeners(rewriteLinks);
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
        const h = treeElement.querySelectorAll(MINIHIERARCHY);
        h.forEach(hierarchy => {
            showSelected(hierarchy);
        });
    }

    function createExpandListeners(rewriteFunction) {
        // add a single listener for expandable tree nodes
        treeElement.onclick = (e) => {
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
        const cb = selectedEntityCallback;

        parent.querySelectorAll(`a.${type}`).forEach(link => {
            const originalUrl = link.getAttribute("href");
            const entityId = getEntityId(originalUrl);

            // but only refresh the entity part of the page
            link.onclick = function (e) {
                e.preventDefault();
                if (originalUrl) {
                    window.history.pushState({}, '', originalUrl); // make sure URL follows
                }
                selectEntity(link);

                // TODO move url gen out
                const url = "/" + pluralType + "/" + entityId + "/fragment";
                entityPane.loadEntity(url, originalUrl);
                if (cb) {
                    cb(entityId);
                }
            }
        });
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
                        rewriteLinks(throwaway.firstElementChild);
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
                treeElement.innerHTML = "<h4>" + err +"</h4>";
            })

        treeElement.innerHTML = BUSY_IMAGE;
    }

    function selectEntity(element) {
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
        let url = path + 'children?';
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
        let pathElements = originalUrl.split("/");
        return pathElements[pathElements.length - 2];
    }

    return {
        init: init,
        reload: reload,
    };
};