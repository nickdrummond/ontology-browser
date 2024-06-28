export const characteristics = (parentSelector) => {

    const HIDDEN = "hidden characteristics";
    const ICON_HTML = "<img class=\"min\" src=\"" + baseUrl + "static/images/min.png\" width=\"16\" height=\"16\"/>";
    const VALUES_SELECTOR = "ul, table";

    const parentElement = document.querySelector(parentSelector);

    function init(selectors) {
        createSlideToggles(selectors);
        hideCharacteristics();
    }

    function createSlideToggles(selectors) {
        parentElement.querySelectorAll(selectors).forEach(characteristic => {
            const icon = createIconElement();
            icon.onclick = (e) => {
                toggle(characteristic);
            };
            characteristic.insertBefore(icon, characteristic.firstChild);
        });
    }

    function toggle(characteristic) {
        const values = getValuesElement(characteristic);
        const name = getCharacteristicName(characteristic);
        const isVisible = values.checkVisibility();
        if (isVisible) {
            close(characteristic);
        }
        else {
            open(characteristic);
        }
        setCharacteristic(name, !isVisible);
    }

    function getCharacteristicName(characteristicElement) {
        return characteristicElement.classList.value;
    }

    function createIconElement() {
        const throwaway = document.createElement('img');
        throwaway.innerHTML = ICON_HTML;
        return throwaway.firstElementChild;
    }

    function hideCharacteristic(characteristic) {
        const arr = getHidden();
        const isAlreadyHidden = arr.includes(characteristic);
        if (!isAlreadyHidden) {
            arr.push(characteristic);
        }
        sessionStorage.setItem(HIDDEN, JSON.stringify(arr));
    }

    function setCharacteristic(characteristic, visible) {
        const arr = getHidden();
        const isAlreadyHidden = arr.includes(characteristic);
        if (visible && isAlreadyHidden) {
            arr.splice(arr.indexOf(characteristic), 1);
            sessionStorage.setItem(HIDDEN, JSON.stringify(arr));
        }
        else if (!visible && !isAlreadyHidden) {
            arr.push(characteristic);
            sessionStorage.setItem(HIDDEN, JSON.stringify(arr));
        }
    }

    function getHidden() {
        const str = sessionStorage.getItem(HIDDEN);
        return str ? JSON.parse(str) : [];
    }

    function hideCharacteristics() {
        getHidden().forEach( hidden => {
            const sel = "." + hidden.replaceAll(" ", ".");
            parentElement.querySelectorAll(sel).forEach(characteristic => {
               close(characteristic);
            });
        });
    }

    function close(characteristic) {
        const values = getValuesElement(characteristic);
        $(values).slideUp("fast"); // TODO get rid of JQuery
    }

    function open(characteristic) {
        const values = getValuesElement(characteristic);
        $(values).slideDown("fast"); // TODO get rid of JQuery
    }

    function getValuesElement(characteristic) {
        return characteristic.querySelector(VALUES_SELECTOR);
    }

    return {
        init: init
    }
}