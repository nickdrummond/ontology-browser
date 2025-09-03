
export const edits = () => {

    function rememberTransaction() {
        const urlParams = new URLSearchParams(window.location.search);
        const transaction = urlParams.get('transaction');
        if (transaction !== null) {
            sessionStorage.setItem("transaction", transaction);
        }
    }

    function init(selectors) {

        let editForm = document.getElementsByClassName("edit").item(0);

        let editAxiom = document.getElementById("edit-axiom");
        let editOntology = document.getElementById("edit-ontology");
        // let editOriginalOntology = document.getElementById("edit-original-ontology");
        // let editOriginalAxiom = document.getElementById("edit-original-axiom");

        var options = {
            parser: baseUrl + 'parse/axiom',
            autocomplete: baseUrl + 'autocomplete/axiom'
        };
        new ExpressionEditor("edit-axiom", options).initialise();

        // clicking on axioms in characteristics sets the editor
        document.querySelectorAll(selectors).forEach(el => {
            el.querySelectorAll(".asserted").forEach( ax => {
                ax.onclick = () => {
                    const axiom = ax.querySelector(".owlobject").getAttribute("data");
                    const ontology = ax.querySelector(".source-ont").getAttribute("data");
                    editAxiom.value = editAxiom.placeholder = axiom;
                    editOntology.value = ontology;
                    // editOriginalAxiom.value = axiom;
                    // editOriginalOntology.value = ontology;
                    // focus the editor
                    editAxiom.selectionStart = editAxiom.selectionEnd = editAxiom.value.length;
                    editForm.classList.add("active");
                    editAxiom.focus();
                }
            });
        });
    }

    return {
        init,
        rememberTransaction,
    }
}