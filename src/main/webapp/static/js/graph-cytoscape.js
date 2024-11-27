document.addEventListener('DOMContentLoaded', function () {

    let layout = {
        name: 'fcose',
        quality: 'default',
        animate: true,
        animationDuration: 2000,
        fit: true,
        idealEdgeLength: 100,
        numIter: 5000,
    }

    let style = [
        {
            selector: 'node',
            style: {
                'background-color': '#2B65EC',
                'background-opacity': 0,
                'border-width': 1,
                'border-style': 'solid',
                'border-color': '#666',
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
                'border-width': 2,
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

    function setupControls() {
        setupControl("type", (newValue) => {
            layout.name = newValue;
            cy.layout(layout).run();
        });
        setupControl("space", (newValue) => {
            layout.idealEdgeLength = parseInt(newValue);

            console.log("space", layout);

            cy.layout(layout).run();
        });

        setupControl("depth", newValue => reload());
        setupControl("query", newValue => reload());
        setupControl("indivs", newValue => reload());
        setupControl("props", newValue => reload());
        setupControl("follow", newValue => reload());
        setupControl("parents", newValue => reload());
    }

    function reload() {
        console.log("loading graph data...");
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let url = '/graph/data?' + new URLSearchParams(window.location.search).toString();
        console.log(url);
        fetch(url, {
            headers: myHeaders,
        })
            .then(response => {
                response.json().then(json => {
                    console.log("elements", json.elements.length);
                    buildGraph(json.elements);
                });
            });
    }

    function setupControl(name, changed) {
        const ctrl = document.getElementById(name);
        if (ctrl) {
            const type = new URLSearchParams(window.location.search).get(name);
            if (type) {
                ctrl.value = type;
            }
            ctrl.addEventListener('change', function () {
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
        cy = cytoscape({
            container: document.querySelector('.graph'), // container to render in
            elements: elements,
            style: style,
            layout: layout,
        });
    }

    reload();
    setupControls();
});