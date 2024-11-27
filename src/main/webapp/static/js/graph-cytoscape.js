document.addEventListener('DOMContentLoaded', function () {

    let currentLayout;

    const defaultLayout = {
        name: 'fcose',
        quality: 'default',
        animate: true,
        animationDuration: 1000,
        fit: true,
        idealEdgeLength: 100,
        numIter: 5000,
    }

    const layouts = {
        'concentric': {
            name: 'concentric',
            minNodeSpacing: 100,
            concentric: function (node) { // returns numeric value for each node, placing higher nodes in levels towards the centre
                return node.degree();
            }
        },
        'circle': {
            name: 'circle',
            spacingFactor: 1,
        },
        'grid': {
            name: 'grid',
            avoidOverlap: true,
            avoidOverlapPadding: 10,
        }
    };

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

    function getLayout(type) {
        if (!type) {
            return {...defaultLayout};
        }

        if (layouts.hasOwnProperty(type)) {
            const layout = layouts[type];
            return {...defaultLayout, ...layout}
        }

        return {...defaultLayout, ...{name: type}};
    }

    function setLengthProp(newValue) {
        switch(currentLayout.name) {
            case 'fcose': currentLayout.idealEdgeLength = newValue * 2; break;
            case 'euler': currentLayout.springLength = 100 + (newValue * 2); break;
            case 'cola': currentLayout.edgeLength = 50 + (newValue * 2); break;
            case 'concentric': currentLayout.minNodeSpacing = newValue; break;
            case 'dagre': currentLayout.spacingFactor = 0.5 + (newValue === 0 ? 0 : (newValue/50)); break;
            case 'circle': currentLayout.spacingFactor = 0.5 + (newValue === 0 ? 0 : (newValue/50)); break;
            case 'grid': currentLayout.avoidOverlapPadding = newValue * 2; break;
        }
    }

    function setupControls() {
        setupControl("type", (newValue) => {
            currentLayout = getLayout(newValue);
            cy.layout(currentLayout).run();
        });
        setupControl("space", (newValue) => {
            setLengthProp(parseInt(newValue));
            cy.layout(currentLayout).run();
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
        let urlSearchParams = new URLSearchParams(window.location.search);
        let url = '/graph/data?' + urlSearchParams.toString();
        fetch(url, {
            headers: myHeaders,
        })
        .then(response => {
            response.json().then(json => {
                const type = urlSearchParams.get("type");
                buildGraph(type, json.elements);
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

    function buildGraph(type, elements) {
        currentLayout = getLayout(type);
        const spaceCtrl = document.getElementById("space");
        setLengthProp(parseInt(spaceCtrl.value));
        cy = cytoscape({
            container: document.querySelector('.graph'), // container to render in
            elements: elements,
            style: style,
            layout: currentLayout,
        });
    }

    reload();
    setupControls();
});