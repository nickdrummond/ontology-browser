import {updateAddress} from "./url-helper.js";
import {getStyle, isDark, HIGHLIGHTED, HIGHLIGHTED_INCOMING, SELECTED} from "./graph-style.js";
import {getLayout, updateLength, defaultLayout} from "./graph-layouts.js";

// Params
export const QUERY = "query";
export const INDIVIDUALS = "indivs";
export const PROPERTIES = "props";
export const NODE_SPACE = "space";

export function graph(selector, endpoint = '/graph/data') {
    const containerDiv = document.querySelector(selector);
    const nodeCount = document.getElementById("nodeCount");
    const edgeCount = document.getElementById("edgeCount");

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
            const lower = value.toLowerCase();
            const sel = cy.nodes().filter(node =>
                node.data('label')?.toLowerCase().includes(lower)
            );
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
        // Dispatch custom event for refocus
        const event = new CustomEvent('graph:refocus', {
            detail: allSelected.map(s => s.data())
        });
        containerDiv.dispatchEvent(event);
        reload();
    }

    function expandSelected() {
        const selected = cy.$(SELECTED);
        selected.forEach(s => expand(s));
    }

    function handleDoubleClick(event, originalEvent) {
        const node = originalEvent.target;
        expand(node);
    }

    function expand(node) {
        const data = node.data();
        const label = data.label;
        const urlParams = new URLSearchParams(window.location.search);
        if (data.type === 'individual') {
            const current = urlParams.get(INDIVIDUALS)?.trim().split(",") || [];
            console.log("Expanding node " + node.data().label, current);
            if (!current.includes(label)) {
                console.log("Adding to URL param " + INDIVIDUALS + " value " + label);
                current.push(label);
                const newValue = current.join(",");
                updateAddress(INDIVIDUALS, newValue);
                // Dispatch event to update controls
                containerDiv.dispatchEvent(new CustomEvent('graph:updateControls', {
                    detail: { type: INDIVIDUALS, value: newValue }
                }));
                append(label, INDIVIDUALS);
            }
        }
        else if (data.type === 'class' || data.type === 'expression') {
            const current = urlParams.get(QUERY)?.trim() || "";
            const classes = current === "" ? [] : current.split(" or ");
            if (!classes.includes(label)) {
                classes.push(label);
                const newValue = classes.join(" or ");
                updateAddress(QUERY, newValue);
                // Dispatch event to update controls
                containerDiv.dispatchEvent(new CustomEvent('graph:updateControls', {
                    detail: { type: QUERY, value: newValue }
                }));
                append(label, QUERY);
            }
        }
    }

    function removeSelected() {
        const selected = cy.$(SELECTED);
        const selectedLabels = selected.map(s => s.data().label);
        const selectedIds = selected.map(s => s.data().id);
        const urlParams = new URLSearchParams(window.location.search);
        const existingIndividuals = urlParams.get(INDIVIDUALS)?.split(",") || [];
        const newIndividuals = existingIndividuals.filter(sel => !selectedLabels.includes(sel));
        const newValue = newIndividuals.join(',');
        updateAddress(INDIVIDUALS, newValue);
        // Dispatch event to update controls
        containerDiv.dispatchEvent(new CustomEvent('graph:updateControls', {
            detail: { type: INDIVIDUALS, value: newValue }
        }));
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
        let url = endpoint + '?' + urlSearchParams.toString();
        fetch(url, {
            headers: myHeaders,
        })
            .then(response => {
                if (!response.ok) {
                    let msg = response.headers.get("X-parse-error");
                    if (msg) {
                        msg = msg.replace(/\+/g, " ");
                        msg = decodeURIComponent(msg);
                        console.error(msg);
                    }
                    else {
                        throw new Error("HTTP error " + response.status + " - " + response.message);
                    }
                }
                response.json().then(json => {
                    const type = urlSearchParams.get("type");
                    buildGraph(type, json.elements, getStyle());
                });
            });
    }

    function append(labels, param = INDIVIDUALS) {
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let urlSearchParams = new URLSearchParams(window.location.search);
        urlSearchParams.set(param, labels);
        urlSearchParams.set("depth", "0");
        console.log("Appending to graph with " + urlSearchParams.toString());
        let url = endpoint + '?' + urlSearchParams.toString();
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
            size = Math.min(size + (node.degree() * 2), 100);
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
        // Dispatch custom event for selection change
        const event = new CustomEvent('graph:selectionChanged', {
            detail: sel.map(s => s.data())
        });
        containerDiv.dispatchEvent(event);
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
        if (nodeCount) {
            nodeCount.innerText = nodes.length;
        }

        if (edgeCount) {
            edgeCount.innerText = edges.length;
        }
    }

    function buildGraph(type, elements, style) {
        currentLayout = getLayout(type);
        const urlParams = new URLSearchParams(window.location.search);
        const edgeType = urlParams.get("edge-type");
        const spaceValue = parseInt(urlParams.get('space')) || 20;
        setLengthProp(spaceValue);
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

                if (edgeType) {
                    cy.style().selector("edge")
                        .style("curve-style", edgeType)
                        .style("width", edgeType === "straight-triangle" ? 5 : 1)
                        .update();
                }

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

                cy.on('doubleTap', 'node', handleDoubleClick);
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

    function getEdgeType() {
        if (!cy) {
            const urlParams = new URLSearchParams(window.location.search);
            return urlParams.get("edge-type") || "straight-triangle";
        }
        return cy.style().selector("edge").style("curve-style");
    }

    function setEdgeType(newValue) {
        if (cy) {
            cy.style().selector("edge")
                .style("curve-style", newValue)
                .style("width", newValue === "straight-triangle" ? 5 : 1)
                .update();
            cy.layout(currentLayout).run();
        }
    }

    return {
        setCurrentLayout,
        getCurrentLayout,
        getEdgeType,
        setEdgeType,
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