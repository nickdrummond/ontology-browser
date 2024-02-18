export const classToggler = (normal, alt) => {

    const key = "language"; // session storage key

    let targetSelector = "body"; // default whole page

    let toggleSelector = "";

    function attachTo(selector) {
        toggleSelector = selector;
        const lang = retrieve();
        const notLang = (lang === alt) ? normal : alt;
        $(toggleSelector)
            .addClass(notLang)
            .removeClass(lang)
            .click(() => setLanguage((retrieve() === alt) ? normal : alt))
            .html(notLang);
    }

    function withTargetSelector(selector) {
        targetSelector = selector;
        setLanguage((retrieve() === alt) ? alt : normal);
        return this;
    }

    function setLanguage(lang) {
        store(lang);
        const notLang = (lang === alt) ? normal : alt;
        $(targetSelector).addClass(lang).removeClass(notLang);
        $(toggleSelector).addClass(notLang).removeClass(lang).html(notLang);
    }

    function store(lang) {
        sessionStorage.setItem(key, lang);
    }

    function retrieve() {
        return sessionStorage.getItem(key);
    }

    return {
        attachTo: attachTo,
        withTargetSelector: withTargetSelector
    }
}