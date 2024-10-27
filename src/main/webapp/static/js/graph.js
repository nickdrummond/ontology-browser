$(document).ready(function(){
    document.querySelectorAll('.graph').forEach( graph => {
        init(graph);
    });
});

let edges = [];

function registerExpandClicks(g) {
    g.onclick = (e) => {
        const th = e.target.closest(".g-edge");
        const node = e.target.closest(".g-node");
        const entityIRI = node.getAttribute("data");
        const urlParams = new URLSearchParams(window.location.search);
        urlParams.append("iri", entityIRI);
        const url = "/graph/fragment?" + urlParams.toString();
        fetch(url)
            .then(response => {
                response.text().then(html => {
                    const dummy = document.createElement("div");
                    dummy.innerHTML = html;
                    let expandedNode = dummy.firstElementChild;
                    edges.forEach(oldEdge => {
                        oldEdge.remove();
                    });
                    edges = [];
                    // TODO somehow remove duplicate nodes (but edges still need to remain)
                    th.replaceWith(expandedNode);
                    init(g);
                    expandedNode.classList.add("selected");
                });
            })
            .catch(err => {
                // TODO
            });
        return false;
    };
}

function renderAllEdges(g) {
    g.querySelectorAll(".g-th").forEach(element => {
        const target = element.querySelector(`:scope > .g-lr > .g-target`);
        renderEdges(element, target, "top");
        renderEdges(element, target, "bottom");
        renderEdges(element, target, "left");
        renderEdges(element, target, "right");
    });
}

function init(g) {
    renderAllEdges(g);
    registerExpandClicks(g);
}

function getOpposite(location) {
    switch(location) {
        case "top": return "bottom";
        case "bottom": return "top";
        case "left": return "right";
        case "right": return "left";
    }
}

function isHorizontal(location) {
    return location === "left" || location === "right";
}

function renderEdges(element, target, location) {
    const sel = (isHorizontal(location))
        ? ":scope > .g-lr > ." + location + " > li.g-edge"
        : ":scope > ." + location + " > li.g-edge";

    element.querySelectorAll(sel).forEach(edge => {
        const predicate = edge.querySelector(`:scope > .g-predicate`);
        const edgeLabel = predicate.getAttribute("data");
        const subject = edge.querySelector(`:scope > .g-subject`);
        if (subject) { // incoming
            edges.push(new LeaderLine(subject, target, {
                path: 'fluid',
                color: '#93b9c5',
                size: 2,
                startSocket: getOpposite(location),
                endSocket: location,
                startLabel: edgeLabel,
            }));
        }
        else { // outgoing
            const object = edge.querySelector(`:scope > .g-th > .g-lr > .g-node`);
            edges.push(new LeaderLine(target, object, {
                path: 'fluid',
                color: '#4189a0',
                size: 2,
                startSocket: location,
                endSocket: getOpposite(location),
                endLabel: edgeLabel,
            }));
        }
    });
}