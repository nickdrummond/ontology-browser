
export const edits = () => {

    function rememberTransaction() {
        const urlParams = new URLSearchParams(window.location.search);
        const transaction = urlParams.get('transaction');
        if (transaction !== null) {
            sessionStorage.setItem("transaction", transaction);
        }
    }

    function init(selectors) {

        let editAxiom = document.getElementById("edit-axiom");
        let editOntology = document.getElementById("edit-ontology");
        let editOriginalOntology = document.getElementById("edit-original-ontology");
        let editOriginalAxiom = document.getElementById("edit-original-axiom");

        // focus the editor
        editAxiom.selectionStart = editAxiom.selectionEnd = editAxiom.value.length;
        editAxiom.focus();

        // clicking on axioms in characteristics sets the editor
        document.querySelectorAll(selectors).forEach(el => {
            el.querySelectorAll(".asserted").forEach( ax => {
                ax.onclick = () => {
                    const axiom = ax.querySelector(".owlobject").getAttribute("data");
                    const ontology = ax.querySelector(".source-ont").getAttribute("data");
                    editOriginalAxiom.value = axiom;
                    editAxiom.value = editAxiom.placeholder = axiom;
                    editOriginalOntology.value = ontology;
                    editOntology.value = ontology;
                    editAxiom.selectionStart = editAxiom.selectionEnd = editAxiom.value.length;
                    editAxiom.focus();
                }
            });
        });
    }

    return {
        init: init,
        rememberTransaction: rememberTransaction,
    }
}