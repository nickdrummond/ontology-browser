const INDIVIDUALS = "indivs";
const SELECTED = ':selected';
const HIGHLIGHTED = 'highlighted';
const HIGHLIGHTED_INCOMING = 'highlighted-incoming';
const QUERY = "query";
const PROPERTIES = "props";

function getTheme() {
    return document.documentElement.getAttribute(THEME_ATTRIBUTE);
}

document.addEventListener('DOMContentLoaded', function () {

    const containerDiv = document.querySelector('.graph');
    const controls = document.querySelector('.graph-controls');

    if (!containerDiv) {
        console.error("No container div with class 'graph' found");
        return;
    }

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
                'color': '#999999',
                'background-color': '#999999',
                'label': 'data(label)',
                'font-size': '10',
                'min-zoomed-font-size': 25, // optimisation for large graphs
            },
        },

        {
            selector: 'node.' + HIGHLIGHTED,
            style: {
                'color': '#edb0b0',
                'background-color': '#edb0b0',
                'font-size': '12',
                'min-zoomed-font-size': 20, // optimisation for large graphs
                'z-index': 500,
            },
        },

        {
            selector: 'node' + SELECTED,
            style: {
                'color': '#000000',
                'background-color': '#2e5cde',
                'font-size': '14',
                'min-zoomed-font-size': 20, // optimisation for large graphs
                'z-index': 1000,
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
                'line-opacity': 0.2,
                'width': 5,
                'curve-style': 'straight-triangle',
                'label': 'data(label)',
                'font-size': '8',
                'text-opacity': 1,
                'min-zoomed-font-size': 35, // optimisation for large graphs
            },
        },

        { // highlight edges connected to selected nodes
            selector: 'edge.' + HIGHLIGHTED,
            style: {
                'color': '#000000',
                'line-color': '#2e5cde',
                'line-opacity': 0.7,
                'min-zoomed-font-size': 30, // optimisation for large graphs
            },
        },

        { // highlight edges connected to selected nodes
            selector: 'edge.' + HIGHLIGHTED_INCOMING,
            style: {
                'color': '#000000',
                'line-color': '#edb0b0',
                'line-opacity': 1,
                'min-zoomed-font-size': 30, // optimisation for large graphs
            },
        },

        {
            selector: 'edge' + SELECTED,
            style: {
                'color': '#000000',
                'line-color': '#2e5cde',
                'line-opacity': 1,
                'min-zoomed-font-size': 30, // optimisation for large graphs
                // cannot use z-index to bring edges forward
            },
        },
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
                    const inverted = invertHex(value.substring(1));
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

    function fit(sel) {
        cy.stop(true);
        cy.animate({
            fit: {eles: sel, padding: 50},
        }, {
            duration: 1000
        });
    }

    let disableManualSelectionListener = false;

    function search(value) {
        disableManualSelectionListener = true;
        cy.$(SELECTED).unselect();
        if (value !== "") {
            const sel = cy.elements(`[label *= "${value}"]`); // starts with
            sel.select();
            updateSelected(sel);
            fit(sel);
        } else {
            updatedSelectedList([]);
            cy.reset();
        }
        disableManualSelectionListener = false;
    }


    const controlConfig = [
        {id: "depth", defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: QUERY, defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: INDIVIDUALS, defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: PROPERTIES, defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: "incoming", defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: "without", defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: "follow", defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: "parents", defaultValue: null, onChange: () => reload(), onEdit: null},
        {id: "graph-search", defaultValue: null, onChange: null, onEdit: newValue => search(newValue)},
        {
            id: "type", defaultValue: defaultLayout.name, onChange: (newValue) => {
                currentLayout = getLayout(newValue);
                cy.layout(currentLayout).run();
            }, onEdit: null,
        },
        {
            id: "space", defaultValue: null, onChange: (newValue) => {
                setLengthProp(parseInt(newValue));
                cy.layout(currentLayout).run();
            }, onEdit: null,
        },
    ];

    document.getElementById("refocus").onclick = (e) => {
        e.preventDefault();
        const allSelected = cy.$(SELECTED);

        // Set the components and then update the address once from the controls
        const instances = allSelected
            .filter(s => s.data().type === 'individual')
            .map(s => s.data().label).join(',');
        document.getElementById(INDIVIDUALS).value = instances;
        updateAddress(INDIVIDUALS, instances);

        const classes = allSelected
            .filter(s => s.data().type === 'class')
            .map(s => s.data().label).join(' or '); // build union of classes
        document.getElementById(QUERY).value = classes;
        updateAddress(QUERY, classes);

        const properties = allSelected
            .filter(s => Boolean(s.data().source))
            .map(s => s.data().label).join(',');
        document.getElementById(PROPERTIES).value = properties;
        updateAddress(PROPERTIES, properties);

        reload();
    };

    document.getElementById("expand").onclick = (e) => {
        e.preventDefault();
        const selected = cy.$(SELECTED);
        const selectedLabels = selected.map(s => s.data().label);
        const current = document.getElementById(INDIVIDUALS);
        const merged = unionSet(selectedLabels, current.value.split(","));
        current.value = merged.join(',');
        append(selectedLabels);
    };

    document.getElementById("delete").onclick = (e) => {
        e.preventDefault();
        const selected = cy.$(SELECTED);
        const selectedLabels = selected.map(s => s.data().label);
        const selectedIds = selected.map(s => s.data().id);
        const current = document.getElementById(INDIVIDUALS);
        const existingIndividuals = current.value.split(",");
        const newIndividuals = existingIndividuals.filter(sel => !selectedLabels.includes(sel));
        current.value = newIndividuals.join(',');
        remove(selectedLabels, selectedIds);
    };

    document.getElementById("png").onclick = (e) => {
        e.preventDefault();
        const png64 = cy.png({
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

    function openBase64InNewTab(data, mimeType) {
        const byteCharacters = atob(data);
        const byteNumbers = new Array(byteCharacters.length);
        for (let i = 0; i < byteCharacters.length; i++) {
            byteNumbers[i] = byteCharacters.charCodeAt(i);
        }
        const byteArray = new Uint8Array(byteNumbers);
        const file = new Blob([byteArray], {type: mimeType + ';base64'});
        const fileURL = URL.createObjectURL(file);
        window.open(fileURL);
    }

    window.addEventListener('popstate', function () {
        controlConfig.forEach(ctrl => {
            setupControlValue(ctrl.id, ctrl.defaultValue);
        });
        reload();
    });

    function updateAddress(name, value) {
        if (value === "") {
            value = null;
        }

        const params = new URLSearchParams(window.location.search);
        const current = params.get(name);
        if (current === value) {
            return;
        }
        if (value) {
            params.set(name, value);
        } else {
            params.delete(name);
        }
        window.history.pushState({}, '', window.location.pathname + '?' + params);
    }

    function setupControlValue(id, defaultValue) {
        const ctrl = document.getElementById(id);
        if (ctrl) {
            const valueFromUrl = new URLSearchParams(window.location.search).get(id);
            if (valueFromUrl) {
                ctrl.value = valueFromUrl;
            } else if (defaultValue) {
                ctrl.value = defaultValue;
            } else {
                ctrl.value = null;
            }
        }
    }

    function setupControlListeners(id, onChange, onEdit) {
        const ctrl = document.getElementById(id);
        if (ctrl) {
            if (onChange) {
                ctrl.addEventListener('change', () => {
                    updateAddress(id, ctrl.value);
                    onChange(ctrl.value);
                });
            }
            if (onEdit) {
                ctrl.addEventListener('keyup', () => {
                    onEdit(ctrl.value);
                });
            }
        }
    }

    function reload() {
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let urlSearchParams = new URLSearchParams(window.location.search);
        document.title = urlSearchParams.get(INDIVIDUALS);
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

    function append(labels) {
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let urlSearchParams = new URLSearchParams(window.location.search);
        urlSearchParams.set(INDIVIDUALS, labels);
        urlSearchParams.set("depth", "0");
        let url = '/graph/data?' + urlSearchParams.toString();
        fetch(url, {
            headers: myHeaders,
        })
            .then(response => {
                response.json().then(json => {
                    const elements = json.elements;
                    // Remove duplicates that already occur in the graph
                    // Use the ID as that is unique hash of the edge or node
                    const filtered = elements.filter(element => {
                        const existing = cy.getElementById(element.data.id);
                        return (!existing.length > 0);
                    });

                    cy.add(filtered);
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
        cy.elements().removeClass(HIGHLIGHTED).removeClass(HIGHLIGHTED_INCOMING);
        if (sel.length === 1) {
            sel.map(element => {
                if (element.isNode()) {
                    const outgoing = cy.edges("[source='" + element.id() + "']");
                    outgoing.addClass(HIGHLIGHTED);
                    // node highlighted if one step from selected node
                    outgoing.map(edge => edge.target().addClass(HIGHLIGHTED));
                    const incoming = cy.edges("[target='" + element.id() + "']");
                    incoming.addClass(HIGHLIGHTED_INCOMING);
                    // node highlighted if one step from selected node
                    incoming.map(edge => edge.source().addClass(HIGHLIGHTED));
                } else if (element.isEdge()) {
                    element.source().addClass(HIGHLIGHTED);
                    element.target().addClass(HIGHLIGHTED);
                }
            });
        } else if (sel.length > 1) {
            // if more than one node selected, only highlight edges between them
            for (let i = 0; i < sel.length; i++) {
                if (sel[i].isEdge()) {
                    continue;
                }
                for (let j = i + 1; j < sel.length; j++) {
                    if (sel[j].isEdge()) {
                        continue;
                    }
                    // otherwise they must be nodes
                    const a = sel[i].id();
                    const b = sel[j].id();
                    cy.edges("[source='" + a + "'][target='" + b + "']").addClass(HIGHLIGHTED);
                    cy.edges("[source='" + b + "'][target='" + a + "']").addClass(HIGHLIGHTED);
                }
            }
        }
    }

    const expand = (event, originalEvent) => {
        const node = originalEvent.target;
        const sel = node.data().label;
        const indivsId = INDIVIDUALS;
        const indivs = document.getElementById(indivsId);
        const current = indivs.value.split(",");
        if (!current.includes(sel)) {
            current.push(sel);
            indivs.value = current.join(",");
            append(sel);
            updateAddress(indivsId, indivs.value);
        }
    }

    function selectWithFocus(node) {
        disableManualSelectionListener = true;
        cy.$(SELECTED).unselect();
        node.select();
        updateSelected([node]);
        fit(node);
        disableManualSelectionListener = false;
    }

    function pluralType(type) {
        switch (type) {
            case 'individual':
                return 'individuals';
            case 'class':
                return 'classes';
            case 'objectproperty':
                return 'objectproperties';
            case 'dataproperty':
                return 'dataproperties';
            default:
                return '';
        }
    }

    function updatedSelectedList(sel) {
        const selectedList = document.getElementById("selected-nodes");
        while (selectedList.firstChild) {
            selectedList.removeChild(selectedList.lastChild);
        }
        sel.forEach(node => {
            const li = document.createElement("li");
            li.textContent = node.data().label + " ";
            // link to the entity page
            const pageLink = document.createElement("a");
            pageLink.href = baseUrl + pluralType(node.data().type) + '/' + node.data().entityId;
            pageLink.target = "_blank";
            pageLink.textContent = "↗";
            li.append(pageLink);
            li.onclick = () => {
                selectWithFocus(node)
            };
            selectedList.appendChild(li);
        });
    }

    let lastSelected = "";
    const updateSelected = (sel) => {
        if (sel) {
            const ids = JSON.stringify(sel.map(s => s.data().id));
            highlightConnectedEdges(sel);
            if (ids === lastSelected) {
                return;
            }
            lastSelected = ids;
            updatedSelectedList(sel);
        }
    }

    function updateStats(nodes, edges) {
        const nodeCount = document.getElementById("nodeCount");
        if (nodeCount) {
            nodeCount.innerText = nodes.length;
        }

        const edgeCount = document.getElementById("edgeCount");
        if (edgeCount) {
            edgeCount.innerText = edges.length;
        }
    }

    function buildGraph(type, elements, style) {
        currentLayout = getLayout(type);
        const spaceCtrl = document.getElementById("space");
        setLengthProp(parseInt(spaceCtrl.value));
        if (cy) {
            cy.off('select');
            cy.off('unselect');
            cy.off('tap');
            // try cy.removeAllListeners();
            cy.destroy(); // need to destroy old instance to avoid memory leaks
        }

        cy = cytoscape({
            container: containerDiv, // container to render in
            ready: function () {
                const nodes = this.nodes();
                const edges = this.edges();

                updateStats(nodes, edges);

                nodes.forEach(function (node) {
                    render(node);
                });
                let sel = this.$(SELECTED);
                updateSelected(sel);
            },
            elements: elements,
            style: style,
            layout: currentLayout,
        });

        // Prevent the listeners from being triggered when we select/unselect programmatically - eg search
        cy.on('select', () => {
            if (!disableManualSelectionListener) {
                updateSelected(cy.$(SELECTED));
            }
        });

        cy.on('unselect', () => {
            if (!disableManualSelectionListener) {
                updateSelected(cy.$(SELECTED));
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
    }

    reload();
    controlConfig.forEach(ctrl => {
        setupControlValue(ctrl.id, ctrl.defaultValue);
        setupControlListeners(ctrl.id, ctrl.onChange, ctrl.onEdit);
    });

    // On any resize of the controls (because of the sel list), resize the graph
    new ResizeObserver(() => {
        if (cy) {
            cy.resize();
        }
    }).observe(controls);
});