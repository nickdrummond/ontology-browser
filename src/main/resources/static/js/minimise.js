export const minimise = (component) => {

    function addMinimise() {
        let classList = component.className;
        const name = classList.replace(" hidden", "");
        const hidden = sessionStorage.getItem(name);
        if (hidden && hidden === "true") {
            component.classList.add("hidden");
        }
        const trigger = component.querySelector(".hide-trigger");
        if (trigger) {
            trigger.onclick = (e) => {
                component.classList.toggle("hidden");
                sessionStorage.setItem(name, component.classList.contains("hidden"));
            };
        }
    }

    return {
        addMinimise,
    }
}