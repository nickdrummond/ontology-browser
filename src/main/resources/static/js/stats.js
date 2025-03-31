document.addEventListener('DOMContentLoaded', function () {

    const charts = [];

    const DEFAULT_IMPORTS = "EXCLUDED";

    const renderToggle = (ontId, statsHolder, importsIncluded) => {
        const included = "<label for='imports-included'>included</label><input id='imports-included' class='imports-selector' type='radio' name='imports' value='INCLUDED'" + ((importsIncluded === "INCLUDED") ? " checked" : "") + "/>";
        const excluded = "<label for='imports-excluded'>excluded</label><input id='imports-excluded' class='imports-selector' type='radio' name='imports' value='EXCLUDED'" + ((importsIncluded === "EXCLUDED") ? " checked" : "") + "/>";
        const html = "<span>imports " + included + excluded + "</span>";

        statsHolder.insertAdjacentHTML("beforeBegin", html);
        document.querySelectorAll(".imports-selector").forEach(element => {
            element.onclick = (e) => {
                let importsIncluded = e.target.value;
                const searchParams = new URLSearchParams(window.location.search);
                searchParams.set("imports", importsIncluded);
                window.history.pushState({}, "", "?" + searchParams.toString());
                load(ontId, importsIncluded);
            }
        });
    }

    const init = () => {
        const statsHolder = document.querySelector(".stats-holder");
        if (statsHolder) {
            const ontId = statsHolder.getAttribute("data-ont-id");
            const importsIncluded = new URLSearchParams(window.location.search).get('imports') ?? DEFAULT_IMPORTS;
            renderToggle(ontId, statsHolder, importsIncluded);
            load(ontId, importsIncluded);
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
    const load = (ontId, importsIncluded) => {
        clear();
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let url = '/stats?imports=' + importsIncluded + "&" + getOntIdQuery(ontId);
        fetch(url, {
            headers: myHeaders,
        })
            .then(response => {
                response.json()
                    .then(json => {
                        render(json, ontId);
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

    const render = (stats, ontId) => {
        let entityCounts = stats.entityCounts;
        const labels = Object.keys(entityCounts);
        const data = Object.values(entityCounts);
        createPie(".pie-class-vs-inst", "Class or instance", labels.slice(0, 2), data.slice(0, 2), {
            circumference: 180,
        });
        createPie(".pie-properties", "Property types", labels.slice(2, 5), data.slice(2, 5), {
            circumference: 180,
            rotation: 90, // upside-down
            infoPosition: "bottom",
        });

        let axiomCounts = stats.axiomCounts;
        createPie(".pie-axiom-types", "Axiom types", Object.keys(axiomCounts), Object.values(axiomCounts), {
            circumference: 180,
            getURL: (label) => "/axioms/?" + getOntIdQuery(ontId) + "&type=" + label, // TODO needs the ontID
        });

        let axiomTypeCounts = stats.axiomTypeCounts.counts;
        createPie(".pie-axiom-type-counts", "Logical axiom types", Object.keys(axiomTypeCounts), Object.values(axiomTypeCounts), {
            showLegend: false,
            infoPosition: "bottom",
            getURL: (label) => "/axioms/?" + getOntIdQuery(ontId) + "&type=" + label, // TODO needs the ontID
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
                        title: {
                            display: true,
                            text: title,
                            position: options.infoPosition,
                        },
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