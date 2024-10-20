

$(document).ready(function(){
    document.querySelectorAll('.graph').forEach( graph => {
        init(graph);
    });
});

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
            new LeaderLine(subject, target, {
                path: 'fluid',
                color: '#93b9c5',
                size: 2,
                startSocket: getOpposite(location),
                endSocket: location,
                startLabel: edgeLabel,
            });
        }
        else { // outgoing
            const object = edge.querySelector(`:scope > .g-th > .g-lr > .g-node`);
            new LeaderLine(target, object, {
                path: 'fluid',
                color: '#4189a0',
                size: 2,
                startSocket: location,
                endSocket: getOpposite(location),
                endLabel: edgeLabel,
            });
        }
    });
}

function init(g) {
    g.querySelectorAll(".g-th").forEach(element => {
        const target = element.querySelector(`:scope > .g-lr > .g-target`);
        renderEdges(element, target, "top");
        renderEdges(element, target, "bottom");
        renderEdges(element, target, "left");
        renderEdges(element, target, "right");
    })
}