document.addEventListener('DOMContentLoaded', function () {

    const charts = [];

    const renderToggle = (ontId, statsHolder, imports) => {
        const included = "<input id='imports-included' class='imports-selector' type='checkbox' name='imports' " + ((imports === "INCLUDED") ? " checked" : "") + "/>";
        const label = " <label for='imports-included'>Include imports</label>"
        const html = "<span>" + included + label + "</span>";

        statsHolder.insertAdjacentHTML("afterBegin", html);

        document.querySelectorAll(".imports-selector").forEach(element => {
            element.onclick = (e) => {
                let imports = e.target.checked ? "INCLUDED" : "EXCLUDED";
                const searchParams = new URLSearchParams(window.location.search);
                searchParams.set("imports", imports);
                window.history.pushState({}, "", "?" + searchParams.toString());
                load(ontId, imports);
            }
        });
    }

    const init = () => {
        const statsHolder = document.querySelector(".stats-holder");
        if (statsHolder) {
            const ontId = statsHolder.getAttribute("data-ont-id");
            const imports = statsHolder.getAttribute("data-imports-included");
            const importControl = statsHolder.getAttribute("data-import-control");
            if (importControl === null || importControl !== "false") {
                renderToggle(ontId, statsHolder, imports);
            }
            load(ontId, imports);
        }
    }

    const content = document.getElementById("main");
    new MutationObserver((mutationList, observer) => {
        for (const mutation of mutationList) {
            if (mutation.type === "childList") {
                init();
            }
        }}).observe(content, {childList: true});

    const clear = () => {
        charts.forEach(chart => chart.destroy());
    };

    // ontId is not defined = doesn't work for / (as we need to explicitly create in the page - which is back to horrid)
    const load = (ontId, imports) => {
        clear();
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let url = '/stats?imports=' + imports + "&" + getOntIdQuery(ontId);
        fetch(url, {
            headers: myHeaders,
        })
            .then(response => {
                response.json()
                    .then(json => {
                        render(json, ontId, imports);
                    });
            });
    };

    const defaultOptions = {
        circumference: 360,
        rotation: 270,
        showLegend: true,
        infoPosition: "top",
        isLog: false,
        getURL: (label) => "/" + label.toLowerCase() + "/?" + getOntIdQuery(),
    };

    const render = (stats, ontId, imports) => {
        let entityCounts = stats.entityCounts;
        const labels = Object.keys(entityCounts);
        const data = Object.values(entityCounts);
        createPie(".pie-class-vs-inst", "Class or instance", labels.slice(0, 2), data.slice(0, 2), {
            // circumference: 180,
            infoPosition: "bottom",
        });
        createPie(".pie-properties", "Property types", labels.slice(2, 5), data.slice(2, 5), {
            // circumference: 180,
            // rotation: 90, // upside-down
            infoPosition: "bottom",
        });

        let axiomCounts = stats.axiomCounts;
        createPie(".pie-axiom-types", "Axiom types", Object.keys(axiomCounts), Object.values(axiomCounts), {
            // circumference: 180,
            infoPosition: "bottom",
            getURL: (label) => "/axioms/?" + getOntIdQuery(ontId) + "&imports=" + imports + "&type=" + label,
        });

        let axiomTypeCounts = stats.axiomTypeCounts.counts;
        createPie(".pie-axiom-type-counts", "Logical axiom types", Object.keys(axiomTypeCounts), Object.values(axiomTypeCounts), {
            showLegend: false,
            // infoPosition: "bottom",
            // circumference: 180,
            // rotation: 90, // upside-down
            getURL: (label) => "/axioms/?" + getOntIdQuery(ontId) + "&imports=" + imports + "&type=" + label,
            isLog: true,
        });

        // let childCountDistribution = stats.classChildCountDistribution;
        // createScatter("classChildCount", "Class child count", childCountDistribution);
    };

    const getOntIdQuery = (ontId) => {
        if (ontId) {
            return "ontId=" + ontId;
        }
        return "";
    };


    const createPie = (selector, title, labels, data, options) => {
        const canvas = document.querySelector(selector);
        if (canvas === null) {
            console.error("Cannot find canvas for chart: " + selector);
            return;
        }

        options = {...defaultOptions, ...options};

        let displayData = data;

        if (options.isLog) {
            data = data.map(x => x === 0 ? 0 : Math.log(x));
        }

        const chart = new Chart(canvas,
            {
                type: "doughnut",
                data: {
                    labels: labels,
                    datasets: [{
                        data: data,
                    }],
                },
                options: {
                    // ...options, could but then have to nest
                    responsive: false,
                    plugins: {
                        legend: {
                            display: options.showLegend,
                            position: options.infoPosition,
                        },
                        tooltip: {
                            callbacks: {
                                label: function (context) {
                                    return displayData[context.dataIndex];
                                }
                            }
                        },
                    },
                    rotation: options.rotation,
                    circumference: options.circumference,
                }
            });

        canvas.onclick = function (evt) {
            const activePoints = chart.getElementsAtEventForMode(evt, 'nearest', {intersect: true}, true)[0];
            const index = labels[activePoints.index];
            window.location.href = options.getURL(index);
        };

        canvas.onmousemove = function (evt) {
            const activePoints = chart.getElementsAtEventForMode(evt, 'nearest', {intersect: true}, true)[0];
            if (activePoints) {
                canvas.style.cursor = 'pointer';
            } else {
                canvas.style.cursor = 'default';
            }
        }

        charts.push(chart);
    }

    // function createScatter(selector, title, coords) {
    //     const canvas = document.getElementById(selector);
    //     const chart = new Chart(canvas,
    //         {
    //             type: "scatter",
    //             data: {
    //                 datasets: [{
    //                     label: title,
    //                     data: coords,
    //                 }],
    //             },
    //             options: {
    //                 responsive: false,
    //                 plugins: {
    //                     title: {
    //                         display: true,
    //                         text: title,
    //                     },
    //                     legend: {
    //                         display: false,
    //                         // position: "bottom",
    //                     }
    //                 },
    //             }
    //         }
    //     );
    // }

    init();
});