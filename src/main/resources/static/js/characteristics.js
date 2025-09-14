export const characteristics = (parentSelector) => {

    const HIDDEN_CLASS = "hidden";
    const HIDDEN_CHARACTERISTIC_KEY = "hidden characteristics";

    const parentElement = document.querySelector(parentSelector);

    function init(selectors) {
        hideCharacteristics();
        createSlideToggles(selectors);
    }

    function createSlideToggles(selectors) {
        parentElement.querySelectorAll(selectors).forEach(characteristic => {
            let trigger = characteristic.querySelector(".hide-trigger");
            trigger.onclick = (e) => {
                console.log("clicked");
                // Add animation back in when element is interacted with
                const ul = characteristic.querySelector("ul, .table-wrapper");
                if (ul) {
                    ul.style.transition = "all 0.3s ease-in-out"; // add the animation after initial hide
                }
                toggle(characteristic);
            };
        });
    }

    function toggle(characteristic) {
        const classList = characteristic.classList;
        classList.toggle(HIDDEN_CLASS);
        const name = getCharacteristicName(characteristic);
        setCharacteristic(name, !classList.contains(HIDDEN_CLASS));
    }

    function getCharacteristicName(characteristicElement) {
        return characteristicElement.classList.value.replace(" " + HIDDEN_CLASS, "");
    }

    function setCharacteristic(characteristic, visible) {
        const arr = getHidden();
        const isAlreadyHidden = arr.includes(characteristic);
        if (visible && isAlreadyHidden) {
            arr.splice(arr.indexOf(characteristic), 1);
            sessionStorage.setItem(HIDDEN_CHARACTERISTIC_KEY, JSON.stringify(arr));
        }
        else if (!visible && !isAlreadyHidden) {
            arr.push(characteristic);
            sessionStorage.setItem(HIDDEN_CHARACTERISTIC_KEY, JSON.stringify(arr));
        }
    }

    function getHidden() {
        const str = sessionStorage.getItem(HIDDEN_CHARACTERISTIC_KEY);
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
        characteristic.classList.add(HIDDEN_CLASS);
    }

    return {
        init: init
    }
}