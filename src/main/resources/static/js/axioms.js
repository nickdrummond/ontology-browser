import {minimise} from './minimise.js';
import { importsToggle } from './imports-toggle.js';

document.addEventListener("DOMContentLoaded", () => {
    const nav = document.getElementsByClassName("owlselector primary")[0];
    minimise(nav).addMinimise();

    const parent = document.getElementsByClassName("imports-placeholder")[0];
    const currentValue = new URLSearchParams(window.location.search).get("imports") || "INCLUDED";
    importsToggle().renderToggle(parent, currentValue, (newValue) => {
        window.location.reload();
    })
});
