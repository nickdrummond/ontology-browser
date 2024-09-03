export const theme = (normal, alt, attribute, key) => {

    let component;

    function not(lang) {
        return (lang === alt) ? normal : alt;
    }

    function attachTo(selector) {
        component = document.querySelector(selector);
        const lang = retrieve();
        component.addEventListener("click", () => setLanguage(not(retrieve())));
        toggle(lang);
    }

    function toggle(lang) {
        const notLang = not(lang);
        component.classList.add(notLang);
        component.classList.remove(lang);
        component.innerHTML = notLang;
    }

    function setLanguage(lang) {
        store(lang);
        document.documentElement.setAttribute(attribute, lang);
        toggle(lang);
    }

    function store(lang) {
        sessionStorage.setItem(key, lang);
    }

    function retrieve() {
        return sessionStorage.getItem(key);
    }

    return {
        attachTo: attachTo
    }
}