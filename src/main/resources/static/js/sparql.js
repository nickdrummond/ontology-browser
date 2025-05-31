import {entity} from "./entity.js";

document.addEventListener("DOMContentLoaded", function(event) {
    entity("#content").openLinksInEntityPane(document.getElementById("resultsForm"), "td a");
});