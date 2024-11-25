document.addEventListener('DOMContentLoaded', function(){
    LeaderLine.positionByWindowResize = false;
    document.querySelectorAll('.graph').forEach( graph => {
        init(graph);
    });
});

function init(g) {
    elmWrapper = document.createElement("div");
    elmWrapper.style.position = 'relative';
    elmWrapper.style.height = '0';
    elmWrapper.style.width = '0';
    g.parentNode.append(elmWrapper);
    console.log("elm", g, g.parentNode, elmWrapper);
    renderAllEdges(g);
    registerExpandClicks(g);
}

let elmWrapper = null;
let currentLines = [];
const externalLines = [];

function findMatchingNode(url, g) {
    return g.querySelector(`[data='${url}']`);
}

function zoneForPredicate(edgeLabel) {
    const params = new URLSearchParams(window.location.search);
    const entries = params.entries();
    for (const entry of entries) {
        if (entry[1].split(",").includes(edgeLabel)) {
            return entry[0];
        }
    }
    throw "Cannot find a zone for property " + edgeLabel;
}

function merge(subGraph, g) {
    const externalLines = [];
    const target = subGraph.querySelector(":scope > .center > .g-target");

    subGraph.querySelectorAll(".g-edge").forEach(edge => {
        const predicate = edge.querySelector(`:scope > .g-predicate`);
        let subject = target;
        let object = edge.querySelector(".g-target");
        let toRemove = object;
        if (!object) {
            subject = edge.querySelector(".g-subject");
            object = target;
            toRemove = subject;
        }

        const subjectUrl = subject.getAttribute("data");
        const objectUrl = object.getAttribute("data");
        const edgeLabel = predicate.getAttribute("data");
        const existingNode = findMatchingNode(toRemove.getAttribute("data"), g);
        if (existingNode) {
            externalLines.push({
                subject: subjectUrl,
                predicate: edgeLabel,
                object: objectUrl,
                zone: zoneForPredicate(edgeLabel),
            });
            // remove its edge from the subgraph
            toRemove.closest(".g-edge").remove();
        }
    });
    return externalLines;
}

function clearLines() {
    document.querySelectorAll('.leader-line').forEach(line => {
        document.body.appendChild(line);
    });
    currentLines.forEach(oldEdge => {
        oldEdge.remove();
    });
    currentLines = [];
}

function responseDOMFor(html) {
    const dummy = document.createElement("div");
    dummy.innerHTML = html;
    return dummy.firstElementChild;
}

function handleResponse(response, g, th) {
    response.text().then(html => {
        let expandedNode = responseDOMFor(html);
        externalLines.push(...merge(expandedNode, g));
        th.replaceWith(expandedNode);
        renderAllEdges(g);
        expandedNode.classList.add("selected");
    });
}

function registerExpandClicks(g) {
    g.onclick = (e) => {
        console.log("click", e);
        const th = e.target.closest(".g-edge");
        const node = e.target.closest(".g-node");
        const entityIRI = node.getAttribute("data");
        const urlParams = new URLSearchParams(window.location.search);
        urlParams.append("iri", entityIRI);
        const url = `/graph/fragment?${urlParams.toString()}`;
        fetch(url)
            .then(response => {
                handleResponse(response, g, th);
            })
            .catch(err => {
                // TODO
            });
        return false;
    };
}

function setupScroller() {
    if (elmWrapper) {
        console.log("Scroll", elmWrapper);

        function position() {
            elmWrapper.style.transform = 'none';
            const rectWrapper = elmWrapper.getBoundingClientRect();
            // Move to the origin of coordinates as the document
            console.log("rect", rectWrapper.left, rectWrapper.top, scrollX, scrollY);
            elmWrapper.style.transform = 'translate(' +
                ((rectWrapper.left + scrollX) * -1) + 'px, ' +
                ((rectWrapper.top + scrollY) * -1) + 'px)';
        }

        document.querySelectorAll('.leader-line').forEach(line => {
            elmWrapper.appendChild(line);
        });
        position();
    }
}

function renderAllEdges(g) {
    clearLines();
    g.querySelectorAll(".g-th").forEach(element => {
        const target = element.querySelector(":scope > .zone.center > .g-target");
        renderEdges(element, target, "top");
        renderEdges(element, target, "bottom");
        renderEdges(element, target, "left");
        renderEdges(element, target, "right");
    });
    externalLines.forEach(line => {
        let subjectNode = findMatchingNode(line.subject, g);
        let objectNode = findMatchingNode(line.object, g);
        addLine(subjectNode, line.predicate, objectNode, line.zone);
    });
    setupScroller();
}

function getOpposite(location) {
    switch(location) {
        case "top": return "bottom";
        case "bottom": return "top";
        case "left": return "right";
        case "right": return "left";
    }
}

function addLine(subject, predicate, object, zone) {
    let line = new LeaderLine(subject, object, {
        path: 'fluid',
        color: '#418910',
        size: 2,
        startSocket: zone,
        endSocket: getOpposite(zone),
        endLabel: predicate,
    });
    currentLines.push(line);
}

function renderEdge(edge, target, zone) {
    const predicate = edge.querySelector(`:scope > .g-predicate`);
    const edgeLabel = predicate.getAttribute("data");
    const subject = edge.querySelector(`:scope > .g-subject`);
    if (subject) { // incoming
        addLine(subject, edgeLabel, target, getOpposite(zone));
    } else { // outgoing
        const object = edge.querySelector(`:scope > .g-th > .zone > .g-node`);
        addLine(target, edgeLabel, object, zone);
    }
}

function renderEdges(element, target, location) {
    const sel = `:scope > .zone.${location} > .g-edge`;
    element.querySelectorAll(sel).forEach(edge => {
        renderEdge(edge, target, location);
    });
}