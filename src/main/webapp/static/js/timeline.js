

$(document).ready(function(){
    let timeline = document.getElementsByClassName('events');
    if(timeline.length > 0) {
        init(timeline[0]);
    }
});

function init(timeline) {
    timeline.querySelectorAll("li.event.dot.faint").forEach(faint => {
        const iri = faint.getAttribute("data");
        timeline.querySelectorAll(`li.event.dot[data="${iri}"]`).forEach(target => {
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
    })
}