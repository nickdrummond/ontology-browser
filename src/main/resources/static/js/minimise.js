export const minimise = (component) => {

    function addMinimise() {
        console.log("Adding minimise to component", component.className);
        let classList = component.className;
        const name = classList.replace(" hidden", "");
        const hidden = sessionStorage.getItem(name);
        if (hidden && hidden === "true") {
            component.classList.add("hidden");
        }
        const title = component.querySelector("h4.header .title"); // TODO make this a selector too?
        if (title) {
            title.onclick = (e) => {
                component.classList.toggle("hidden");
                sessionStorage.setItem(name, component.classList.contains("hidden"));
            };
        }
    }

    return {
        addMinimise,
    }
}