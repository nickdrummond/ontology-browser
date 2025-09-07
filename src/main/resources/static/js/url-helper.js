export function updateAddress(name, value) {
    if (value === "") {
        value = null;
    }

    const params = new URLSearchParams(window.location.search);
    const current = params.get(name);
    if (current === value) {
        return;
    }
    if (value) {
        params.set(name, value);
    } else {
        params.delete(name);
    }
    window.history.pushState({}, '', window.location.pathname + '?' + params);
}
