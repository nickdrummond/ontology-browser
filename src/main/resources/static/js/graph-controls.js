import {updateAddress} from "./url-helper.js";

export const QUERY = "query";
export const INDIVIDUALS = "indivs";
export const PROPERTIES = "props";

export const graphControls = (selector, graph) => {
    const controls = document.querySelector(selector);
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

    const controlConfig = [
        {id: "depth", defaultValue: null, onChange: () => graph.reload(), onEdit: null},
        {id: QUERY, defaultValue: null, onChange: () => graph.reload(), onEdit: null},
        {id: INDIVIDUALS, defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/individuals'},
        {id: PROPERTIES, defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
        {id: "incoming", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
        {id: "without", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
        {id: "follow", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
        {id: "parents", defaultValue: null, onChange: () => graph.reload(), onEdit: null, autocomplete: 'autocomplete/properties'},
        {id: "graph-search", defaultValue: null, onChange: null, onEdit: newValue => graph.search(newValue)},
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
    ];

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

    controlConfig.forEach(ctrl => {
        setupControlValue(ctrl.id, ctrl.defaultValue);
        setupControlListeners(ctrl.id, ctrl.onChange, ctrl.onEdit);
        setupControlAutocomplete(ctrl.id, ctrl.autocomplete);
    });

    document.getElementById("refocus").onclick = (e) => {
        e.preventDefault();
        graph.refocus();
    }

    document.getElementById("expand").onclick = (e) => {
        e.preventDefault();
        graph.expandSelected();
    }

    document.getElementById("delete").onclick = (e) => {
        e.preventDefault();
        graph.removeSelected();
    }

    document.getElementById("png").onclick = (e) => {
        e.preventDefault();
        const png64 = graph.exportPng();
        openBase64InNewTab(png64, 'image/png');
    }

    document.getElementById("save").onclick = (e) => {
        e.preventDefault();
        console.log("Saving graph layout to local storage");
        graph.saveToLocal();
    }

    document.getElementById("recover").onclick = (e) => {
        e.preventDefault();
        console.log("Recovering graph layout from local storage");
        graph.recoverFromLocal();
    }

    window.addEventListener('popstate', function () {
        controlConfig.forEach(ctrl => {
            setupControlValue(ctrl.id, ctrl.defaultValue);
        });
        graph.reload();
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

    return {
        getContainer: () => controls,
    }
}