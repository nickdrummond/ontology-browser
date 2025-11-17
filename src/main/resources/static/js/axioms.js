import { importsToggle } from './imports-toggle.js';

document.addEventListener("DOMContentLoaded", () => {
    const parent = document.getElementsByClassName("imports-placeholder")[0];
    const currentValue = new URLSearchParams(window.location.search).get("imports") || "INCLUDED";
    importsToggle().renderToggle(parent, currentValue, (newValue) => {
        window.location.reload();
    })
});
