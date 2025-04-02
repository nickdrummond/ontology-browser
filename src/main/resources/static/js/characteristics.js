export const characteristics = (parentSelector) => {

    const HIDDEN = "hidden characteristics";
    const VALUES_SELECTOR = "ul, table";

    const parentElement = document.querySelector(parentSelector);

    function init(selectors) {
        hideCharacteristics();
        createSlideToggles(selectors);
    }

    function createSlideToggles(selectors) {
        parentElement.querySelectorAll(selectors).forEach(characteristic => {
            let firstChild = characteristic.querySelector("h4");
            firstChild.onclick = (e) => {
                toggle(characteristic);
            };
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
        return characteristicElement.classList.value.replace(" hidden", "");
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
               closeInstantly(characteristic);
            });
        });
    }

    function closeInstantly(characteristic) {
        const values = getValuesElement(characteristic);
        values.style.display = "none";
        characteristic.classList.add("hidden");
    }

    function close(characteristic) {
        const values = getValuesElement(characteristic);
        $(values).slideUp("fast"); // TODO get rid of JQuery
        characteristic.classList.add("hidden");
    }

    function open(characteristic) {
        const values = getValuesElement(characteristic);
        $(values).slideDown("fast"); // TODO get rid of JQuery
        characteristic.classList.remove("hidden");
    }

    function getValuesElement(characteristic) {
        return characteristic.querySelector(VALUES_SELECTOR);
    }

    return {
        init: init
    }
}