export const SELECTED = ':selected';
export const HIGHLIGHTED = 'highlighted';
export const HIGHLIGHTED_INCOMING = 'highlighted-incoming';

// Light/dark style support must be under control of cytoscape for png export to work
export function getStyle() {
    return getTheme() === 'dark' ? invert(lightStyle) : lightStyle;
}

export function isDark() {
    return getTheme() === 'dark';
}

function getTheme() {
    return document.documentElement.getAttribute(THEME_ATTRIBUTE);
}

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


const lightStyle = [
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