document.addEventListener('DOMContentLoaded', function () {

    let currentLayout;

    const defaultLayout = {
        name: 'cola',
    }

    const layouts = {
        'fcose': {
            name: 'fcose',
            idealEdgeLength: 50,
        },
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
        },
        'cola': {
            name: 'cola',
        },
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
                'label': 'data(label)',
                'font-size': '10',
            }
        },

        {
            selector: ':parent',
            style: {
                'background-opacity': 0.02,
                'border-color': '#2B65EC',
                'font-size': '12',
            }
        },

        {
            selector: 'edge',
            style: {
                'line-color': 'rgba(175,175,175,0.12)',
                'width': 1,
                'curve-style': 'bezier',//'straight-triangle',
                'label': 'data(label)',
                'font-size': '8',
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
        setupControl("type", defaultLayout.name, (newValue) => {
            currentLayout = getLayout(newValue);
            cy.layout(currentLayout).run();
        });
        setupControl("space", null, (newValue) => {
            setLengthProp(parseInt(newValue));
            cy.layout(currentLayout).run();
        });

        setupControl("depth", null, newValue => reload());
        setupControl("query", null, newValue => reload());
        setupControl("indivs", null, newValue => reload());
        setupControl("props", null, newValue => reload());
        setupControl("without", null, newValue => reload());
        setupControl("follow", null, newValue => reload());
        setupControl("parents", null, newValue => reload());
    }

    function reload() {
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

    function setupControl(name, defaultValue, changed) {
        const ctrl = document.getElementById(name);
        if (ctrl) {
            const type = new URLSearchParams(window.location.search).get(name);
            if (type) {
                ctrl.value = type;
            }
            else if (defaultValue) {
                ctrl.value = defaultValue;
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

    function getShape(node) {
        switch(node.data().type) {
            case 'individual': return "diamond";
            case 'class': return "ellipse";
        }
    }

    function buildGraph(type, elements) {
        currentLayout = getLayout(type);
        const spaceCtrl = document.getElementById("space");
        setLengthProp(parseInt(spaceCtrl.value));
        cy = cytoscape({
            container: document.querySelector('.graph'), // container to render in
            ready: function() {
                this.nodes().forEach(function (node) {
                    let size = Math.min(10 + (node.degree() * 10), 100);
                    node.css("width", size);
                    node.css("height", size);
                    node.css("shape", getShape(node));
                });
            },
            elements: elements,
            style: style,
            layout: currentLayout,
        });
    }

    reload();
    setupControls();
});