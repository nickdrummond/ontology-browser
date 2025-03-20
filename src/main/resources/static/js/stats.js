let Chart = require('./libs/chart.umd.js');

document.addEventListener('DOMContentLoaded', function () {

    console.log("stats");

    function render(stats) {
        console.log("render");

        const labels = stats.entityCounts.keys;
        console.log("labels", labels);
        const data = stats.entityCounts.values;
        console.log("data", data);

        new Chart(document.getElementById('stats'),
            {
                type: "doughnut",
                data: {
                    labels: labels,
                    datasets: [{
                        backgroundColor: ["#FF6384", "#36A2EB", "#64d197", "#4BC0C0", "#532304",  "#FFCE56"],
                        data: data,
                    }],
                },
                options: {
                    title: {
                        display: true,
                        text: "Entity types"
                    }
                }
            });
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

    reload();
});
