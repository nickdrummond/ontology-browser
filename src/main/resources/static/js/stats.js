document.addEventListener('DOMContentLoaded', function () {

    const defaultOptions = {
        circumference: 360,
        rotation: 270,
        showLegend: true,
        infoPosition: "top",
        getURL: (label) => "/" + label.toLowerCase() + "/",
        isLog: false,
    }

    function createPie(selector, title, labels, data, options) {
        const canvas = document.getElementById(selector);

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
    }

    function render(stats) {
        let entityCounts = stats.entityCounts;
        const labels = Object.keys(entityCounts);
        const data = Object.values(entityCounts);
        createPie("classVsInst", "Class or instance", labels.slice(0, 2), data.slice(0, 2), {
            circumference: 180,
        });
        createPie("properties", "Property types", labels.slice(2, 5), data.slice(2, 5), {
            circumference: 180,
            rotation: 90, // upside-down
            infoPosition: "bottom",
        });

        let axiomCounts = stats.axiomCounts;
        createPie("axiomTypes", "Axiom types", Object.keys(axiomCounts), Object.values(axiomCounts), {
            circumference: 180,
            getURL: (label) => "/axioms/?type=" + label,
        });

        let axiomTypeCounts = stats.axiomTypeCounts.counts;
        createPie("axiomTypeCounts", "Logical axiom types", Object.keys(axiomTypeCounts), Object.values(axiomTypeCounts), {
            showLegend: false,
            infoPosition: "bottom",
            getURL: (label) => "/axioms/?type=" + label,
            isLog: true,
        });

        // let childCountDistribution = stats.classChildCountDistribution;
        // createScatter("classChildCount", "Class child count", childCountDistribution);
    }

    function reload() {
        const myHeaders = new Headers();
        myHeaders.append("Accept", "application/json");
        let urlSearchParams = new URLSearchParams(window.location.search);
        let url = '/stats' + urlSearchParams.toString();
        fetch(url, {
            headers: myHeaders,
        })
            .then(response => {
                response.json()
                    .then(json => {
                        render(json)
                    });
            });
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

    reload();
});