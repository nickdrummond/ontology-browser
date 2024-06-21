$(document).ready(function () {
    let events = document.getElementsByClassName('events');
    if (events.length > 0) {
        init(events[0]);
    }
});

function init(events) {
    joinAfter(events);
    joinFaint(events);
}

function joinAfter(events) {
    let timelines = events.querySelectorAll("ul.timeline");
    timelines.forEach(t => {
        // walk the timeline
        joinTimeline(t);
    });
}

// TODO if last in timeline, in parallel

function getPropType(prop) {
    if (prop.classList.contains("sometimeAfter")) {
        console.log("sometimeAfter");
        return "sometimeAfter";
    }
    else {
        return "after";
    }
}

function joinTimeline(t) {
    let child = t.firstElementChild;
    let next = child;
    let prop = null;
    while(child && next) {
        while (child?.classList.contains("prop")) {
            child = child.nextElementSibling;
        }

        if (child) {
            next = child.nextElementSibling;
            while (next?.classList.contains("prop")) {
                prop = getPropType(next);
                next = next.nextElementSibling;
            }
            if (next) {
                console.log(prop);
                if (child.classList.contains("parallel")) {
                    // console.log("don't converge the parallel");
                }
                else if (next.classList.contains("parallel")) {
                    let first = true;
                    for (let timeline of next.children) {
                        if (timeline.classList.contains("diverge")) {
                            const firstEvent = getFirstEventInTimeline(timeline);
                            if (firstEvent) {
                                let options = first ? {
                                    color: '#09e5ec',
                                    size: 2,
                                    dash: prop === "sometimeAfter",
                                    path: "grid",
                                    startSocket: "right",
                                    endSocket: "left",
                                } : {
                                    color: '#09e5ec',
                                    size: 2,
                                    dash: prop === "sometimeAfter",
                                    path: "grid",
                                    startSocket: "bottom",
                                    endSocket: "left",
                                };
                                new LeaderLine(child, firstEvent, options);
                            }
                        }
                        first = false;
                    }
                }
                else if (next.classList.contains("event")) {
                    new LeaderLine(child, next, {
                        color: '#09e5ec',
                        size: 2,
                        dash: prop === "sometimeAfter",
                        path: "grid",
                        startSocket: "right",
                        endSocket: "left",
                    });
                }
                child = next;
            }
            else {
                // was last one - is this converging?
                if (t.classList.contains("converge")) {
                    const n = getNextEvent(t.parentElement);
                    if (n) {
                        new LeaderLine(child, n, {
                            color: '#09e5ec',
                            size: 2,
                            dash: prop === "sometimeAfter",
                            path: "grid",
                            startSocket: "right",
                            endSocket: "left",
                        });
                    }
                }
            }
        }
    }
}

function getFirstEventInTimeline(timeline) {
    let child = timeline.firstElementChild;
    while (child?.classList.contains("prop")) {
        child = child.nextElementSibling;
    }
    return child;
}

function getNextEvent(node) {
    do {
        node = node.nextElementSibling;
    } while (node?.classList.contains("prop"));
    return node;
}

function joinFaint(events) {
    events.querySelectorAll("li.event.dot.faint").forEach(faint => {
        const iri = faint.getAttribute("data");
        events.querySelectorAll(`li.event.dot[data="${iri}"]`).forEach(target => {
            if (target !== faint) {
                new LeaderLine(faint, target, {
                    color: '#4189a0',
                    size: 2,
                    dash: true,
                    startSocket: "right",
                    endSocket: "left",
                    startSocketGravity: [200, 200],
                    endSocketGravity: [-200, -200],
                });
            }
        })
    });
}