export const classToggler = (normal, alt) => {

    const key = "language"; // session storage key

    let targetSelector = "html"; // default whole page

    let toggleSelector = "";

    function attachTo(selector) {
        toggleSelector = selector;

        setLanguage((retrieve() === alt) ? alt : normal);

        $(toggleSelector).click(function(e){
            setLanguage((retrieve() === alt) ? normal : alt);
        });
    }

    function withTargetSelector(selector) {
        targetSelector = selector;
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