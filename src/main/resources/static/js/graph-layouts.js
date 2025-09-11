export function getLayout(type) {
    if (!type) {
        return {...defaultLayout};
    }

    if (layouts.hasOwnProperty(type)) {
        const layout = layouts[type];
        return {...defaultLayout, ...layout}
    }

    return {...defaultLayout, ...{name: type}};
}

export const defaultLayout = {
    name: 'cola',
    animationDuration: 5000,
}

export const layouts = {
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
    'klay': {
        name: 'klay',
        klay: {
        }
    }
};

export function updateLength(currentLayout, newValue) {
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
        case 'klay':
            currentLayout.klay.spacing = 20 + newValue;
            break;
    }
}