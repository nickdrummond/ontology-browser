document.addEventListener('DOMContentLoaded', function () {

    let currentLayout;

    const defaultLayout = {
        name: 'cola',
        animationDuration: 10000,
    }

    const layouts = {
        'breadthfirst': {
            name: 'breadthfirst',
            directed: true,
            circle: true,
        },
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
        'dagre' : {
            name: 'dagre',
            rankDir : 'RL',
        },
    };

    let style = [
        {
            selector: 'node',
            style: {
                'background-color': '#999',
                // 'background-opacity': 0,
                // 'border-width': 1,
                // 'border-style': 'solid',
                // 'border-color': '#999',
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
                'color': '#888',
                'line-color': '#000',
                'line-opacity': 0.12,
                'width': 5,
                'curve-style': 'straight-triangle',
                'label': 'data(label)',
                // 'target-label': 'data(label)',
                // 'target-text-offset': 20,
                'font-size': '8',
                'text-opacity': 1,
            }
        },

        {
            selector: 'node:selected',
            style: {
                'background-color': '#F08080',
                // 'border-width': 2,
                // 'border-color': 'red'
            }
        },

        {
            selector: 'edge:selected',
            style: {
                'line-color': '#F08080',
                'line-opacity': 1,
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
            case 'breadthfirst': currentLayout.spacingFactor = 0.5 + (newValue === 0 ? 0 : (newValue/50)); break;
            case 'grid': currentLayout.avoidOverlapPadding = newValue * 2; break;
        }
    }

    function unionSet(a, b) {
        return [...new Set(a).union(new Set(b))];
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

        const refocus = document.getElementById("refocus");
        refocus.onclick = (e) => {
            e.preventDefault();
            const sel = cy.$(':selected').map(s => s.data().label).join(',');
            const indivs = document.getElementById("indivs");
            indivs.value = sel;
            update("indivs", indivs, () => reload());
        };

        const expand = document.getElementById("expand");
        expand.onclick = (e) => {
            e.preventDefault();
            let selected = cy.$(':selected');
            const selectedLabels = selected.map(s => s.data().label);
            const selectedIds = selected.map(s => s.data().id);
            const current = document.getElementById("indivs");
            const merged = unionSet(selectedLabels, current.value.split(","));
            current.value = merged.join(',');
            append(selectedLabels, selectedIds);
        };

        const png = document.getElementById("png");
        png.onclick = (e) => {
            e.preventDefault();
            let png64 = cy.png({
                output: 'base64'
            });
            openBase64InNewTab(png64, 'image/png');
            // document.querySelector('#png-eg').setAttribute('src', png64);
        };

        const save = document.getElementById("save");
        save.onclick = (e) => {
            e.preventDefault();
            const params = new URLSearchParams(window.location.search);
            localStorage.setItem(params.toString(), JSON.stringify(cy.json()));
        };

        const recover = document.getElementById("recover");
        recover.onclick = (e) => {
            e.preventDefault();
            const params = new URLSearchParams(window.location.search);
            const saved = localStorage.getItem(params.toString());
            if (saved != null) {
                cy.json(JSON.parse(saved));
            }
        }
    }

    function openBase64InNewTab (data, mimeType) {
        var byteCharacters = atob(data);
        var byteNumbers = new Array(byteCharacters.length);
        for (var i = 0; i < byteCharacters.length; i++) {
            byteNumbers[i] = byteCharacters.charCodeAt(i);
        }
        var byteArray = new Uint8Array(byteNumbers);
        var file = new Blob([byteArray], { type: mimeType + ';base64' });
        var fileURL = URL.createObjectURL(file);
        window.open(fileURL);
    }

    function update(name, ctrl, changed) {
        const params = new URLSearchParams(window.location.search);
        params.set(name, ctrl.value);
        window.history.pushState({}, '', window.location.pathname + '?' + params);
        if (cy) {
            changed(ctrl.value);
        }
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
                update(name, ctrl, changed);
            });
        }
    }

    function getValue(name) {
        const ctrl = document.getElementById(name);
        return ctrl.value;
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

    function append(labels, ids) {
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let urlSearchParams = new URLSearchParams(window.location.search);
        urlSearchParams.set("indivs", labels);
        urlSearchParams.set("depth", 0);
        let url = '/graph/data?' + urlSearchParams.toString();
        fetch(url, {
            headers: myHeaders,
        })
            .then(response => {
                response.json().then(json => {
                    let elements = json.elements;

                    // remove existing edges that have the requested nodes as source
                    // to prevent duplicates
                    ids.forEach(id => {
                        cy.remove('edge[source="' + id + '"]');
                    })

                    cy.add(elements);
                    cy.layout(currentLayout).run();
                    cy.ready();
                });
            });
    }

    function getShape(node) {
        switch(node.data().type) {
            case 'individual': return "ellipse";
            case 'class': return "rectangle";
            case 'expression': return "octagon";
        }
    }

    function render(node) {
        let size = 10
        if (node.data().type === 'individual') {
            size = Math.min(size + (node.degree() * 10), 100);
            // node.css('background-opacity', 1);
        }
        node.css("width", size);
        node.css("height", size);
        node.css("shape", getShape(node));
    }

    function buildGraph(type, elements) {
        currentLayout = getLayout(type);
        const spaceCtrl = document.getElementById("space");
        setLengthProp(parseInt(spaceCtrl.value));
        cy = cytoscape({
            container: document.querySelector('.graph'), // container to render in
            ready: function() {
                this.nodes().forEach(function (node) {
                    render(node);
                });
            },
            elements: elements,
            style: style,
            layout: currentLayout,
        });
        const doubleClickDelayMs = 300;
        let previousTapStamp;
        cy.on('tap', function(e) {
            const currentTapStamp = e.timeStamp;
            const msFromLastTap = currentTapStamp - previousTapStamp;

            if (msFromLastTap < doubleClickDelayMs) {
                e.target.trigger('doubleTap', e);
            }
            previousTapStamp = currentTapStamp;
        });

        cy.on('doubleTap', 'node', function(event, originalEvent) {
            const node = originalEvent.target;
            const sel = node.data().label;
            const id = node.data().id;
            const indivs = document.getElementById("indivs");
            const current = indivs.value.split(",");
            if (!current.includes(sel)) {
                current.push(sel);
                indivs.value = current.join(",");
            }
            append(sel, [id]);
        });
    }

    reload();
    setupControls();
});