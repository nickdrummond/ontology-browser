import {entity} from "./entity.js";

document.addEventListener("DOMContentLoaded", function(event) {
    const resultsForm = document.getElementById("resultsForm");
    if (resultsForm) {
        entity("#content").openLinksInEntityPane(resultsForm, "td a");
    }
});