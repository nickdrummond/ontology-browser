document.addEventListener('DOMContentLoaded', function () {

    let layout = {
        name: 'fcose',
        quality: 'default',
        animate: true,
        fit: true,
        idealEdgeLength: 100,
    }

    let style = [
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
    ];

    let cy = null;

    let urlParams = new URLSearchParams(window.location.search);

    setupControl("type", (newValue) => {
        layout.name = newValue;
        cy.layout(layout).run();
    });

    setupControl("depth", newValue => {
        reload();
    });

    function reload() {
        console.log("Reloading");
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        fetch('/graph/data?' + urlParams.toString(), {
            headers: myHeaders,
        })
            .then(response => {
                response.json().then(json => {
                    buildGraph(json.elements);
                });
            });
    }

    function setupControl(name, changed) {
        const typeControl = document.getElementById(name);
        if (typeControl) {
            const type = urlParams.get(name);
            if (type) {
                typeControl.value = type;
            }
            typeControl.addEventListener('change', function () {
                const params = new URLSearchParams(window.location.search);
                params.set(name, this.value);
                window.history.pushState({}, '', window.location.pathname + '?' + params);

                if (cy) {
                    changed(this.value);
                }
            });
        }
    }

    function buildGraph(elements) {
        // if (cy == null) {
            cy = cytoscape({
                container: document.querySelector('.graph'), // container to render in
                elements: elements,
                style: style,
                layout: layout,
            });
        // }
        // else {
        //     cy.elements = elements;
        // }
    }

    reload();
});