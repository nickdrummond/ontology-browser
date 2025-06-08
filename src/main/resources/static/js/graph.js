const INDIVIDUALS = "indivs";
const SELECTED = ':selected';
const HIGHLIGHTED = 'highlighted';

function getTheme() {
    return document.documentElement.getAttribute(THEME_ATTRIBUTE);
}

document.addEventListener('DOMContentLoaded', function () {

    let currentLayout;

    const defaultLayout = {
        name: 'cola',
        animationDuration: 5000,
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
        'dagre': {
            name: 'dagre',
            rankDir: 'RL',
        },
    };

    let lightStyle = [
        {
            selector: 'node',
            style: {
                'background-color': '#999999',
                'color': '#000000',
                'label': 'data(label)',
                'font-size': '10',
            },
        },

        {
            selector: ':parent',
            style: {
                'background-opacity': 0.02,
                'border-color': '#2B65EC',
                'font-size': '12',
            },
        },

        {
            selector: 'edge',
            style: {
                'color': '#888888',
                'line-color': '#000000',
                'line-opacity': 0.12,
                'width': 5,
                'curve-style': 'straight-triangle',
                'label': 'data(label)',
                'font-size': '8',
                'text-opacity': 1,
            },
        },

        {
            selector: 'node' + SELECTED,
            style: {
                'background-color': '#F08080',
            },
        },

        {
            selector: 'edge' + SELECTED,
            style: {
                'line-color': '#F08080',
                'line-opacity': 1,
            },
        },

        { // highlight edges connected to selected nodes
            selector: 'edge.highlighted',
            style: {
                'line-color': '#F08080',
                'line-opacity': 0.5,
            },
        }
    ];

    // Light/dark style support must be under control of cytoscape for png export to work
    function getStyle() {
        return getTheme() === 'dark' ? invert(lightStyle) : lightStyle;
    }

    // Check if the theme changes
    new MutationObserver(function (mutations) {
        mutations.forEach(function (mutation) {
            if (mutation.attributeName === THEME_ATTRIBUTE) {
                cy.style(getStyle());
            }
        });
    }).observe(document.documentElement, {
        attributes: true,
    });

    // Equivalent of filter(invert(1)) - colors must all be in #RRGGBB format
    function invert(original) {
        const copy = JSON.parse(JSON.stringify(original));
        copy.forEach(s => {
            let style = s.style;
            for (let key in style) {
                let value = style[key];
                if (typeof value === 'string' && value.startsWith('#')) {
                    inverted = invertHex(value.substring(1));
                    style[key] = '#' + inverted;
                }
            }
        });
        return copy;
    }

    function invertHex(hex) {
        return (Number(`0x1${hex}`) ^ 0xFFFFFF).toString(16).substring(1).toUpperCase()
    }

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
        switch (currentLayout.name) {
            case 'fcose':
                currentLayout.idealEdgeLength = newValue * 2;
                break;
            case 'euler':
                currentLayout.springLength = 100 + (newValue * 2);
                break;
            case 'cola':
                currentLayout.edgeLength = 50 + (newValue * 2);
                break;
            case 'concentric':
                currentLayout.minNodeSpacing = newValue;
                break;
            case 'dagre':
                currentLayout.spacingFactor = 0.5 + (newValue === 0 ? 0 : (newValue / 50));
                break;
            case 'circle':
                currentLayout.spacingFactor = 0.5 + (newValue === 0 ? 0 : (newValue / 50));
                break;
            case 'breadthfirst':
                currentLayout.spacingFactor = 0.5 + (newValue === 0 ? 0 : (newValue / 50));
                break;
            case 'grid':
                currentLayout.avoidOverlapPadding = newValue * 2;
                break;
        }
    }

    function unionSet(a, b) {
        return [...new Set(a).union(new Set(b))];
    }

    function isDark() {
        return getTheme() === 'dark';
    }

    function search(value) {
        cy.edges().removeClass(HIGHLIGHTED);
        cy.nodes().unselect();
        if (value !== "") {
            const sel = cy.nodes(`[label *= "${value}"]`); // starts with
            sel.select();
            nodeSelected(sel);
        } else {
            updatedSelectedList([]);
            cy.reset();
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
        setupControl(INDIVIDUALS, null, newValue => reload());
        setupControl("props", null, newValue => reload());
        setupControl("without", null, newValue => reload());
        setupControl("follow", null, newValue => reload());
        setupControl("parents", null, newValue => reload());
        setupControl("graph-search", null, null, newValue => search(newValue));

        const refocus = document.getElementById("refocus");
        refocus.onclick = (e) => {
            e.preventDefault();
            const sel = cy.$(SELECTED).map(s => s.data().label).join(',');
            document.getElementById(INDIVIDUALS).value = sel;
            updateAddress(INDIVIDUALS, sel);
            reload();
        };

        document.getElementById("expand").onclick = (e) => {
            e.preventDefault();
            let selected = cy.$(SELECTED);
            const selectedLabels = selected.map(s => s.data().label);
            const selectedIds = selected.map(s => s.data().id);
            const current = document.getElementById(INDIVIDUALS);
            const merged = unionSet(selectedLabels, current.value.split(","));
            current.value = merged.join(',');
            append(selectedLabels, selectedIds);
        };

        document.getElementById("delete").onclick = (e) => {
            e.preventDefault();
            let selected = cy.$(SELECTED);
            const selectedLabels = selected.map(s => s.data().label);
            const selectedIds = selected.map(s => s.data().id);
            const current = document.getElementById(INDIVIDUALS);
            let existingIndividuals = current.value.split(",");
            const newIndividuals = existingIndividuals.filter(sel => !selectedLabels.includes(sel));
            current.value = newIndividuals.join(',');
            remove(selectedLabels, selectedIds);
        };

        document.getElementById("png").onclick = (e) => {
            e.preventDefault();
            let png64 = cy.png({
                output: 'base64',
                scale: 2,
                bg: isDark() ? '#000000' : '#FFFFFF',
            });
            openBase64InNewTab(png64, 'image/png');
        };

        document.getElementById("save").onclick = (e) => {
            e.preventDefault();
            console.log("Saving graph layout to local storage");
            const params = new URLSearchParams(window.location.search);
            localStorage.setItem(params.toString(), JSON.stringify(cy.json()));
        };

        document.getElementById("recover").onclick = (e) => {
            e.preventDefault();
            console.log("Recovering graph layout from local storage");
            const params = new URLSearchParams(window.location.search);
            const saved = localStorage.getItem(params.toString());
            if (saved != null) {
                cy.json(JSON.parse(saved));
            }
        }
    }

    function openBase64InNewTab(data, mimeType) {
        var byteCharacters = atob(data);
        var byteNumbers = new Array(byteCharacters.length);
        for (var i = 0; i < byteCharacters.length; i++) {
            byteNumbers[i] = byteCharacters.charCodeAt(i);
        }
        var byteArray = new Uint8Array(byteNumbers);
        var file = new Blob([byteArray], {type: mimeType + ';base64'});
        var fileURL = URL.createObjectURL(file);
        window.open(fileURL);
    }

    function updateAddress(name, value) {
        const params = new URLSearchParams(window.location.search);
        params.set(name, value);
        window.history.pushState({}, '', window.location.pathname + '?' + params);
    }

    function setupControl(name, defaultValue, changed, edited) {
        const ctrl = document.getElementById(name);
        if (ctrl) {
            const type = new URLSearchParams(window.location.search).get(name);
            if (type) {
                ctrl.value = type;
            } else if (defaultValue) {
                ctrl.value = defaultValue;
            }
            if (changed) {
                ctrl.addEventListener('change', () => {
                    updateAddress(name, ctrl.value);
                    changed(ctrl.value);
                });
            }
            if (edited) {
                ctrl.addEventListener('keyup', (event) => {
                    edited(ctrl.value);
                });
            }
        }
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
                    buildGraph(type, json.elements, getStyle());
                });
            });
    }

    function append(labels, ids) {
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let urlSearchParams = new URLSearchParams(window.location.search);
        urlSearchParams.set(INDIVIDUALS, labels);
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

    function remove(labels, ids) {
        ids.forEach(id => {
            cy.remove('node[id="' + id + '"]');
        })
    }

    function getShape(node) {
        switch (node.data().type) {
            case 'individual':
                return "ellipse";
            case 'class':
                return "rectangle";
            case 'expression':
                return "octagon";
        }
    }

    function render(node) {
        let size = 10
        if (node.data().type === 'individual') {
            size = Math.min(size + (node.degree() * 10), 100);
        }
        node.css("width", size);
        node.css("height", size);
        node.css("shape", getShape(node));
    }

    function highlightConnectedEdges(sel) {
        cy.edges().removeClass(HIGHLIGHTED);
        sel.map(node => {
            cy.edges("[source='" + node.id() + "']").addClass(HIGHLIGHTED);
        });
    }

    const expand = (event, originalEvent) => {
        const node = originalEvent.target;
        const sel = node.data().label;
        const id = node.data().id;
        const indivsId = INDIVIDUALS;
        const indivs = document.getElementById(indivsId);
        const current = indivs.value.split(",");
        if (!current.includes(sel)) {
            current.push(sel);
            indivs.value = current.join(",");
            append(sel, [id]);
            updateAddress(indivsId, indivs.value);
        }
    }

    function updatedSelectedList(sel) {
        const selectedList = document.getElementById("selected-nodes");
        while (selectedList.firstChild) {
            selectedList.removeChild(selectedList.lastChild);
        }
        sel.forEach(node => {
            const li = document.createElement("li");
            li.textContent = node.data().label;
            li.onclick = () => {
                cy.$(SELECTED).unselect();
                node.select();
                nodeSelected([node]);
            };
            selectedList.appendChild(li);
        });
    }

    const nodeSelected = (sel) => {
        updatedSelectedList(sel);
        highlightConnectedEdges(sel);
        if (sel.length === 1) {
            cy.center(sel);
        }
        if (sel.length > 1) {
            cy.fit(sel);
        }
    }

    function buildGraph(type, elements, style) {
        currentLayout = getLayout(type);
        const spaceCtrl = document.getElementById("space");
        setLengthProp(parseInt(spaceCtrl.value));
        cy = cytoscape({
            container: document.querySelector('.graph'), // container to render in
            ready: function () {
                this.nodes().forEach(function (node) {
                    render(node);
                });
                nodeSelected(this.$(SELECTED));
            },
            elements: elements,
            style: style,
            layout: currentLayout,
        });

        cy.on('click', function (e) {
            if (e.target.cy && e.target.group() === 'nodes') {
                // Give it a sec to allow the selection to update
                setTimeout(() => {
                    nodeSelected(cy.$(SELECTED));
                }, 100);
            }
        });

        const doubleClickDelayMs = 300;
        let previousTapStamp;

        cy.on('tap', function (e) {
            const currentTapStamp = e.timeStamp;
            const msFromLastTap = currentTapStamp - previousTapStamp;

            if (msFromLastTap < doubleClickDelayMs) {
                e.target.trigger('doubleTap', e);
            }
            previousTapStamp = currentTapStamp;
        });

        cy.on('doubleTap', 'node', expand);

        setupControls();
    }

    reload();
});