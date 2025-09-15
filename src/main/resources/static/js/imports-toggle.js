export const importsToggle = () => {

    const renderToggle = (parent, imports, callback) => {
        const included = "<input id='imports-included' class='imports-selector' type='checkbox' name='imports' " + ((imports === "INCLUDED") ? " checked" : "") + "/>";
        const label = " <label for='imports-included'>Include imports</label>"
        const html = "<span>" + included + label + "</span>";

        parent.insertAdjacentHTML("afterBegin", html);

        document.querySelectorAll(".imports-selector").forEach(element => {
            element.onclick = (e) => {
                let imports = e.target.checked ? "INCLUDED" : "EXCLUDED";
                const searchParams = new URLSearchParams(window.location.search);
                searchParams.set("imports", imports);
                window.history.pushState({}, "", "?" + searchParams.toString());
                callback(imports);
            }
        });
    }

    return {
        renderToggle,
    }
}