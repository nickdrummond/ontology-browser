import {graphControls, INDIVIDUALS, PROPERTIES, QUERY} from "./graph-controls.js";
import {updateAddress} from "./url-helper.js";
import {getStyle, isDark, HIGHLIGHTED, HIGHLIGHTED_INCOMING, SELECTED} from "./graph-style.js";
import {getLayout, updateLength, defaultLayout} from "./graph-layouts.js";

document.addEventListener('DOMContentLoaded', function () {

    console.log("Document loaded, initializing graph");
    const g = graph('.graph');
    const controls = graphControls('.graph-controls', g);

    // On any resize of the controls (because of the sel list), resize the graph
    new ResizeObserver(() => {
        if (g.cy) {
            g.cy.resize();
        }
    }).observe(controls.getContainer());
});

export function graph(selector) {
    const containerDiv = document.querySelector(selector);

    if (!containerDiv) {
        console.error("No container with class " + selector + " found");
        return;
    }

    let cy = null;
    let currentLayout = null;

    let disableManualSelectionListener = false;
    let lastSelected = "";
    let selListeners = [];

    reload();

    function onSelectChange(callback) {
        selListeners.push(callback);
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

    function setLengthProp(newValue) {
        if (!cy) {
            return;
        }
        updateLength(currentLayout, newValue);
        cy.layout(currentLayout).run();
    }

    function unionSet(a, b) {
        return [...new Set(a).union(new Set(b))];
    }

    function fit(sel) {
        cy.stop(true);
        cy.animate({
            fit: {eles: sel, padding: 50},
        }, {
            duration: 1000
        });
    }

    function search(value) {
        disableManualSelectionListener = true;
        cy.$(SELECTED).unselect();
        if (value !== "") {
            const sel = cy.elements(`[label *= "${value}"]`); // contains match
            sel.select();
            updateSelected(sel);
            fit(sel);
        } else {
            notifySelectionListeners([]);
            cy.reset();
        }
        disableManualSelectionListener = false;
    }

    function refocus() {
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
    }

    function expandSelected() {
        const selected = cy.$(SELECTED);
        const selectedLabels = selected.map(s => s.data().label);
        const current = document.getElementById(INDIVIDUALS);
        const merged = unionSet(selectedLabels, current.value.split(","));
        current.value = merged.join(',');
        append(selectedLabels);
    }

    function expand(event, originalEvent) {
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

    function removeSelected() {
        const selected = cy.$(SELECTED);
        const selectedLabels = selected.map(s => s.data().label);
        const selectedIds = selected.map(s => s.data().id);
        const current = document.getElementById(INDIVIDUALS);
        const existingIndividuals = current.value.split(",");
        const newIndividuals = existingIndividuals.filter(sel => !selectedLabels.includes(sel));
        current.value = newIndividuals.join(',');
        updateAddress(INDIVIDUALS, current.value);
        remove(selectedLabels, selectedIds);
    }

    function exportPng() {
        return cy.png({
            output: 'base64',
            scale: 2,
            bg: isDark() ? '#000000' : '#FFFFFF',
        });
    }

    function saveToLocal() {
        const params = new URLSearchParams(window.location.search);
        localStorage.setItem(params.toString(), JSON.stringify(cy.json()));
    }

    function recoverFromLocal() {
        const params = new URLSearchParams(window.location.search);
        const saved = localStorage.getItem(params.toString());
        if (saved != null) {
            cy.json(JSON.parse(saved));
        }
    }

    function reload() {
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let urlSearchParams = new URLSearchParams(window.location.search);
        document.title = "Graph: " + (urlSearchParams.get(QUERY) ?? urlSearchParams.get(INDIVIDUALS) ?? urlSearchParams.get(PROPERTIES) ?? "");
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

    function removeOrphans() {
        cy.nodes().forEach(node => {
            const connected = node.connectedEdges();
            if (connected.length === 0) {
                cy.remove(node);
            }
        });
    }

    function remove(labels, ids) {
        ids.forEach(id => {
            cy.remove('node[id="' + id + '"]');
            removeOrphans();
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

    function selectWithFocus(node) {
        disableManualSelectionListener = true;
        cy.$(SELECTED).unselect();
        node.select();
        updateSelected([node]);
        fit(node);
        disableManualSelectionListener = false;
    }

    function notifySelectionListeners(sel) {
        selListeners.forEach(callback => {
            callback(sel);
        });
    }

    function updateSelected(sel) {
        if (sel) {
            const ids = JSON.stringify(sel.map(s => s.data().id));
            highlightConnectedEdges(sel);
            if (ids === lastSelected) {
                return;
            }
            lastSelected = ids;
            notifySelectionListeners(sel);
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

        cytoscape({
            container: containerDiv, // container to render in
            ready: function () {
                cy = this;
                const nodes = this.nodes();
                const edges = this.edges();

                updateStats(nodes, edges);

                nodes.forEach(function (node) {
                    render(node);
                });
                let sel = this.$(SELECTED);
                updateSelected(sel);

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
            },
            elements: elements,
            style: style,
            layout: currentLayout,
        });
    }

    function setCurrentLayout(newValue) {
        currentLayout = getLayout(newValue);
        cy.layout(currentLayout).run();
    }

    function getCurrentLayout() {
        return currentLayout ?? defaultLayout;
    }

    return {
        setCurrentLayout,
        getCurrentLayout,
        setLengthProp,
        reload,
        search,
        selectWithFocus,
        refocus,
        expandSelected,
        removeSelected,
        exportPng,
        saveToLocal,
        recoverFromLocal,
        onSelectChange,
    }
}