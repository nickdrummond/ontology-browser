import {loadEntity} from "./entity.js";
import {BUSY_IMAGE} from "./util.js";

$.fn.exists = function () {
    return this.length !== 0;
}

$.fn.replaceWithPush = function (a) {
    const $a = $(a);

    this.replaceWith($a);
    return $a;
};

const ACTIVE_ENTITY = "active-entity";
const MINIHIERARCHY = '.minihierarchy';

export const tree = (baseUrl, entityLoadedCallback, isRewriteLinks) => {

    function init() {
        scrollTreeToSelection();
        const primaryTree = document.querySelector(".owlselector.primary");
        const secondaryTree = document.querySelector(".owlselector.secondary");
        if (secondaryTree) {
            if (isRewriteLinks) {
                rewriteLinks(secondaryTree);
            }
            createTreeListeners(primaryTree, false);
            createTreeListeners(secondaryTree, isRewriteLinks);
        } else if (primaryTree) {
            if (isRewriteLinks) {
                rewriteLinks(null); // default to the main tree
            }
            createTreeListeners(primaryTree, isRewriteLinks);
        }
    }

    function scrollTreeToSelection() {
        const h = document.querySelectorAll(MINIHIERARCHY);
        h.forEach(hierarchy => {
            const active = hierarchy.querySelectorAll("." + ACTIVE_ENTITY);
            if (active.length > 0) {
                // let js work out getting into the pane
                active.item(0).scrollIntoView(false);
                // then reposition to the middle
                const p = hierarchy.scrollTop;
                if (p > 0) {
                    const h = hierarchy.clientHeight;
                    hierarchy.scrollTo(0, p + (0.5 * h));
                }
            }
        });
    }

    function createTreeListeners(parent, isRewriteLinks) {
        // add a single listener for expandable tree nodes
        parent.onclick = (e) => {
            const t = e.target.closest('span.expandable');
            if (t) {
                handleExpand(t.parentNode, isRewriteLinks);
            }
        };
    }

    function handleExpand(li, isRewriteLinks) {
        const children = li.querySelectorAll("ul");
        if (children.length > 0) {
            $(children).slideToggle('fast'); // TODO remove JQuery slideToggle - see https://codepen.io/jorgemaiden/pen/YgGZMg
        } else {
            li.append(getChildren(li, isRewriteLinks));
        }
    }

    function getChildren(li, isRewriteLinks) {
        const childList = document.createElement('ul');
        childList.innerHTML = `<li>${BUSY_IMAGE}</li>`;

        let query = 'children';
        if (li.closest(MINIHIERARCHY).classList.contains('Individuals')) {
            query = 'instances';
        }

        const nodeUrl = li.querySelector('a').href;
        const nodeUrlPieces = nodeUrl.split('?');
        let url = nodeUrlPieces[0] + query;
        if (nodeUrlPieces[1]) {
            url = url + '?' + nodeUrlPieces[1];
        }

        fetch(url)
            .then(response => {
                response.text().then(html => {
                    const dummy = document.createElement("div");
                    dummy.innerHTML = html;
                    let expandedNode = dummy.firstChild;
                    li.replaceWith(expandedNode); // replace the li with an expanded version
                    if (isRewriteLinks) {
                        rewriteLinks(expandedNode);
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

    function rewriteLinksFor(type, pluralType, parentElement) {
        const parent = parentElement ?? document.querySelector(".owlselector");

        // TODO a single listener for all links - with closest - but already have createTreeListeners
        parent.querySelectorAll(`a.${type}`).forEach(link => {
            let originalUrl = link.getAttribute("href");

            // entity ID is last path element
            // brittle - happens to work for both entity pages and relations pages
            // this depends on URLScheme staying in sync

            let pathElements = originalUrl.split("/");
            let entityId = pathElements[pathElements.length - 2];

            // but only refresh the entity part of the page
            link.onclick = function (e) {
                e.preventDefault();

                // switch selection
                document.querySelectorAll(".owlselector ." + ACTIVE_ENTITY).forEach(activeEntity =>
                    activeEntity.classList.remove(ACTIVE_ENTITY));
                link.classList.add(ACTIVE_ENTITY);

                let ajaxReq = "/" + pluralType + "/" + entityId + "/fragment";

                loadEntity(ajaxReq, originalUrl, entityLoadedCallback);
            }
        });
    }

    return {
        init: init
    };
};