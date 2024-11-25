document.addEventListener('DOMContentLoaded', function () {

    function buildGraph(elements, type) {
        console.log("Building ", type, " graph");

        var cy = cytoscape({
            container: document.querySelector('.graph'), // container to render in

            elements: elements,

            style: [
                {
                    selector: 'node',
                    style: {
                        'background-color': '#2B65EC',
                        'label': 'data(label)'
                    }
                },

                {
                    selector: ':parent',
                    style: {
                        'background-opacity': 0.02,
                        'border-color': '#2B65EC'
                    }
                },

                {
                    selector: 'edge',
                    style: {
                        'line-color': 'rgba(175,175,175,0.12)',
                        'width': 10,
                        'curve-style': 'straight-triangle',
                        'label': 'data(label)'
                    }
                },

                {
                    selector: 'node:selected',
                    style: {
                        'background-color': '#F08080',
                        'border-color': 'red'
                    }
                },

                {
                    selector: 'edge:selected',
                    style: {
                        'line-color': '#F08080'
                    }
                }
            ],
            layout: {
                name: type,
                quality: 'default',
                animate: false,
                fit: true,
                idealEdgeLength: 100,
                // nodeRepulsion: 4500,
                // edgeElasticity: 0.45,
                // nestingFactor: 0.1,
                // gravity: 0.25,
                // gravityRange: 3.8,
                // gravityCompound: 1,
                // gravityRangeCompound: 1.5,
                // numIter: 500,
                // tilingPaddingVertical: 10,
                // tilingPaddingHorizontal: 10,
                // initialEnergyOnIncremental: 0.3,
                // step: "all"
            },
        });
    }

    const myHeaders = new Headers();
    myHeaders.append("Accept", "application/json");
    const urlParams = new URLSearchParams(window.location.search);
    let type = urlParams.get("type");
    if (!type) {
        type = "fcose";
    }
    fetch('/graph/data?' + urlParams.toString(), {
        headers: myHeaders,
    })
        .then(response => {
            response.json().then(json => {
                buildGraph(json.elements, type);
            });
        });
});