export const characteristics = (parentSelector) => {

    const HIDDEN = "hidden characteristics";

    const parentElement = document.querySelector(parentSelector);

    function init(selectors) {
        hideCharacteristics();
        createSlideToggles(selectors);
    }

    function createSlideToggles(selectors) {
        parentElement.querySelectorAll(selectors).forEach(characteristic => {
            let firstChild = characteristic.querySelector("h4");
            firstChild.onclick = (e) => {
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
        classList.toggle("hidden");
        const name = getCharacteristicName(characteristic);
        setCharacteristic(name, !classList.contains("hidden"));
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
        characteristic.classList.add("hidden");
    }

    return {
        init: init
    }
}