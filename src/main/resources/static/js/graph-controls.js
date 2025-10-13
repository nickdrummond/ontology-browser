import {updateAddress} from "./url-helper.js";
import {INDIVIDUALS, PROPERTIES, QUERY} from "./graph.js";

export const Actions = {
    REFOCUS: 'refocus',
    EXPAND: 'expand',
    DELETE: 'delete',
    PNG: 'png',
    SAVE: 'save',
    RECOVER: 'recover',
};

export const graphControls = ({
    selector,
    actions = Object.values(Actions),
    graph,
}) => {
    console.log("Initialising graph controls for selector: " + selector);

    function hookupActionButton(action, actions, onClick) {
        const button = document.getElementById(action.valueOf());
        if (actions.includes(action)) {
            button.onclick = (e) => {
                e.preventDefault();
                onClick();
            }
        }
        else {
            // hide the button
            button.style.display = "none";
        }
    }

    if (actions && actions.length > 0) {
        const controlConfig = [
            {id: "graph-search", defaultValue: null, onChange: null, onEdit: newValue => graph.search(newValue)},
            {id: "depth", defaultValue: null, onChange: () => graph.reload(), onEdit: null},
            {
                id: "type", defaultValue: graph.getCurrentLayout().name, onChange: (newValue) => {
                    graph.setCurrentLayout(newValue);
                }, onEdit: null,
            },
            {
                id: "space", defaultValue: null, onChange: (newValue) => {
                    graph.setLengthProp(parseInt(newValue));
                }, onEdit: null,
            },
            {
                id: "edge-type", defaultValue: graph.getEdgeType(), onChange: (newValue) => {
                    graph.setEdgeType(newValue);
                }, onEdit: null,
            },
        ];

        controlConfig.forEach(ctrl => {
            setupControlValue(ctrl.id, ctrl.defaultValue);
            setupControlListeners(ctrl.id, ctrl.onChange, ctrl.onEdit);
        });

        hookupActionButton(Actions.REFOCUS, actions, () => graph.refocus());
        hookupActionButton(Actions.EXPAND, actions, () => graph.expandSelected());
        hookupActionButton(Actions.DELETE, actions, () => graph.removeSelected());
        hookupActionButton(Actions.PNG, actions, () => {
            const png64 = graph.exportPng();
            openBase64InNewTab(png64, 'image/png');
        });
        hookupActionButton(Actions.SAVE, actions, () => {
            console.log("Saving graph layout to local storage");
            graph.saveToLocal();
        });
        hookupActionButton(Actions.RECOVER, actions, () => {
            console.log("Recovering graph layout from local storage");
            graph.recoverFromLocal();
        });

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
    }

    const controls = document.querySelector(selector);
    if (controls) {
        const controlConfig = [
            {id: QUERY, defaultValue: null, onChange: () => graph.reload(), onEdit: null},
            {id: INDIVIDUALS, defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/individuals'},
            {id: PROPERTIES, defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
            {id: "incoming", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
            {id: "without", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
            {id: "follow", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
            {id: "parents", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
        ];

        const individualsCtrl = document.getElementById(INDIVIDUALS);
        const queryCtrl = document.getElementById(QUERY);
        const propertiesCtrl = document.getElementById(PROPERTIES);

        const selectedList = document.getElementById("selected-nodes");

        new ExpressionEditor(QUERY, {
            parser : baseUrl + 'parse/class-expression',
            autocomplete: baseUrl + 'autocomplete/class-expression'
        }).initialise();

        if (selectedList != null) {
            graph.onSelectChange((sel) => {
                while (selectedList.firstChild) {
                    selectedList.removeChild(selectedList.lastChild);
                }
                sel.forEach(node => {
                    const li = document.createElement("li");
                    li.textContent = node.data().label + " ";
                    // link to the entity page
                    const pluralType = getPluralType(node.data().type);
                    if (pluralType !== '') {
                        const pageLink = document.createElement("a");
                        pageLink.href = baseUrl + pluralType + '/' + node.data().entityId;
                        pageLink.target = "_blank";
                        pageLink.textContent = "↗";
                        li.append(pageLink);
                    }
                    li.onclick = () => {
                        graph.selectWithFocus(node)
                    };
                    selectedList.appendChild(li);
                });
            });
        }
        controlConfig.forEach(ctrl => {
            setupControlValue(ctrl.id, ctrl.defaultValue);
            setupControlListeners(ctrl.id, ctrl.onChange, ctrl.onEdit);
            setupControlAutocomplete(ctrl.id, ctrl.autocomplete);
        });
            // Listen for custom graph events to update controls
        controls.addEventListener('graph:selectionChanged', (event) => {
            const selectedData = event.detail;
            // Update controls based on selected nodes
            const instances = selectedData
                .filter(s => s.type === 'individual')
                .map(s => s.label).join(',');
            individualsCtrl.value = instances;
            updateAddress(INDIVIDUALS, instances);

            const classes = selectedData
                .filter(s => s.type === 'class')
                .map(s => s.label).join(' or ');
            queryCtrl.value = classes;
            updateAddress(QUERY, classes);

            const properties = selectedData
                .filter(s => Boolean(s.source))
                .map(s => s.label).join(',');
            propertiesCtrl.value = properties;
            updateAddress(PROPERTIES, properties);
        });

        controls.addEventListener('graph:refocus', (event) => {
            const selectedData = event.detail;
            // Same logic as selectionChanged for refocus
            const instances = selectedData
                .filter(s => s.type === 'individual')
                .map(s => s.label).join(',');
            individualsCtrl.value = instances;
            updateAddress(INDIVIDUALS, instances);

            const classes = selectedData
                .filter(s => s.type === 'class')
                .map(s => s.label).join(' or ');
            queryCtrl.value = classes;
            updateAddress(QUERY, classes);

            const properties = selectedData
                .filter(s => Boolean(s.source))
                .map(s => s.label).join(',');
            propertiesCtrl.value = properties;
            updateAddress(PROPERTIES, properties);
        });

        // Listen for custom graph events to update controls
        controls.addEventListener('graph:updateControls', (event) => {
            const {type, value} = event.detail;
            if (type === INDIVIDUALS) {
                individualsCtrl.value = value;
                updateAddress(INDIVIDUALS, value);
            } else if (type === QUERY) {
                queryCtrl.value = value;
                updateAddress(QUERY, value);
            } else if (type === PROPERTIES) {
                propertiesCtrl.value = value;
                updateAddress(PROPERTIES, value);
            }
        });

        function getPluralType(type) {
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

    function setupControlAutocomplete(id, autocomplete) {
        if (autocomplete) {
            new ExpressionEditor(id, {
                autocomplete: baseUrl + autocomplete
            }).initialise();
            let ctrl = document.getElementById(id);
            ctrl.setAttribute("placeholder", "Comma-separated. Ctrl-space to autocomplete");
            ctrl.setAttribute("autocomplete", "off");
        }
    }

    return {
        getContainer: () => controls,
    }
}