export const theme = (normal, alt, attribute, key) => {

    let component;

    function not(theme) {
        return (theme === alt) ? normal : alt;
    }

    function attachTo(selector) {
        component = document.querySelector(selector);
        component.addEventListener("click", (event) => {
            event.preventDefault(); // prevent navigation if a link
            setTheme(not(retrieve()));
        });
        toggle(retrieve());
    }

    function toggle(theme) {
        const notTheme = not(theme);
        component.classList.add(notTheme);
        component.classList.remove(theme);
        component.innerHTML = notTheme === "dark" ? "&#9790;" : "&#9788;️";
    }

    function setTheme(theme) {
        document.documentElement.setAttribute(attribute, theme);
        store(theme);
        toggle(theme);
    }

    function store(theme) {
        localStorage.setItem(key, theme);
    }

    function retrieve() {
        return localStorage.getItem(key);
    }

    return {
        attachTo: attachTo
    }
}