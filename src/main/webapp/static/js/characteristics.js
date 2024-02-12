export const characteristics = () => {

    const HIDDEN = "hidden.";

    function init(selectors) {
        createSlideToggles(selectors);
        hideCharacteristics();
    }

    function createSlideToggles(selectors) {
        $("<img class=\"min\" src=\"" + baseUrl + "static/images/min.png\" width=\"16\" height=\"16\"/>").click(function(e){
            const values = $(this).nextAll("ul, table").first(); // for some reason just next does not work
            const hidden = values.is(":visible");
            const characteristic = $(this).next("h4").text();
            rememberCharacteristicHidden(characteristic, hidden);

            values.slideToggle('fast');
        }).prependTo(selectors);
    }

    function rememberCharacteristicHidden(characteristic, hidden) {
        if (hidden) {
            sessionStorage.setItem(HIDDEN + characteristic, true);
        }
        else {
            sessionStorage.removeItem(HIDDEN + characteristic);
        }
    }

    function hideCharacteristics() {
        let keys = Object.keys(sessionStorage);
        for(let key of keys) {
            if (key.startsWith(HIDDEN)) {
                const characteristic = key.substr(HIDDEN.length);
                $("h4:contains('" + characteristic + "')").nextAll("ul, table").first().hide();
            }
        }
    }

    return {
        init: init
    }
}