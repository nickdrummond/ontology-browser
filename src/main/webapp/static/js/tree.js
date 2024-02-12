import {getXmlHttpObject} from "./util.js";

$.fn.exists = function () {
    return this.length !== 0;
}

$.fn.replaceWithPush = function(a) {
    var $a = $(a);

    this.replaceWith($a);
    return $a;
};

export const tree = (baseUrl, entityLoadedCallback) => {

    const BUSY_IMAGE = baseUrl + "static/images/busy.gif";

    function init() {
        scrollTreeToSelection();
        createTreeListeners();
        rewriteLinks(null);
    }

    function scrollTreeToSelection() {
        $(".minihierarchy:visible").each(function() {
            var active = $("span.active-entity", this);
            if (active.size() > 0) {
                // let js work out getting into the pane
                active.get(0).scrollIntoView(false);
                // then reposition to the middle
                var p = $(this).scrollTop();
                if (p > 0) {
                    var h = $(this).height();
                    $(this).scrollTop(p + (0.5 * h));
                }
            }
        });
    }

    function createTreeListeners(){
        // add a single listener for expandable tree nodes
        $(".minihierarchy").click(function(e){
            var t = $(e.target).closest('span.expandable');
            if (t.exists()) {
                handleExpand(t.parent());
            }
        });
    }

    function handleExpand(li){
        var children = $("ul", li);
        if (children.length > 0){
            children.slideToggle('fast');
        }
        else{
            li.append(getChildren(li));
        }
    }

    function getChildren(li){
        var childList = $(`<ul><li><img alt="loading" src="${BUSY_IMAGE}" width="10" height="10"/></li></ul>`);

        var query = 'children';
        if (li.closest('.minihierarchy').hasClass('Individuals')) {
            query = 'instances';
        }

        var nodeUrl = $('a', li).first().attr('href');
        var nodeUrlPieces = nodeUrl.split('?');
        var url = nodeUrlPieces[0] + query;
        if (nodeUrlPieces[1]) {
            url = url + '?' + nodeUrlPieces[1];
        }

        $.ajax({
            url: url,
            context: li,
            success: function(data, textStatus, request){
                const expanded = li.replaceWithPush(data); // replace the li with an expanded version
                rewriteLinks(expanded[0]);
            },
            error: function(request, textStatus, errorThrown){
                // get rid of the spinner and replace with an error message
                console.error(errorThrown);
                $("ul", this).html("Sorry, cannot get children - " + textStatus);
            }
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
        rewriteLinksFor("ontology-uri", "ontologies", parentElement);
    }

    function rewriteLinksFor(type, pluralType, parentElement) {
        const parent = parentElement ?? document.querySelector(".owlselector");

        parent.querySelectorAll(`a.${type}`).forEach(link => {
            let entityId = link.getAttribute("href").split("/")[2];
            // but only refresh the entity part of the page
            link.onclick = function (e) {
                e.preventDefault();
                document.querySelectorAll(".owlselector .active-entity").forEach(activeEntity =>
                    activeEntity.classList.remove("active-entity"));
                link.classList.add("active-entity");
                loadEntity(type.toLowerCase(), pluralType, entityId, link.getAttribute("href"));
            }
        });
    }

    function loadEntity(type, pluralType, entityId, url) {
        if (entityId == null) {
            return;
        }

        const xmlHttpReq = getXmlHttpObject();

        if (xmlHttpReq == null) {
            alert("Browser does not support HTTP Request");
        } else {
            const req = baseUrl + pluralType + "/fragment/" + entityId;

            xmlHttpReq.open("GET", req, true);

            xmlHttpReq.setRequestHeader("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8");

            xmlHttpReq.onload = function () {
                const status = xmlHttpReq.status;
                const response = xmlHttpReq.responseText;

                if (status === 200) { // OK
                    const responseHolder = document.createElement('span');
                    responseHolder.innerHTML = response;
                    document.getElementById("content").replaceWith(responseHolder.firstChild);
                    window.history.pushState({}, '', url); // make sure URL follows
                    // TODO set title
                    entityLoadedCallback();
                } else {
                    resultWrite(type + ": error!", status + ":" + response);
                }
            };

            xmlHttpReq.send();

            document.getElementById("content").innerHTML = `<img src="${BUSY_IMAGE}" width="10px" height="10px" />`;
        }
    }

    return {
        init: init
    };
};