package org.ontbrowser.www.feature.editing;

import org.ontbrowser.www.feature.ontologies.OWLOntologiesService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@ControllerAdvice
@Profile("editing")
public class TransactionOntologyControllerAdvice {

    private final OWLHTMLKit kit;

    private final OWLOntologiesService ontService;

    public TransactionOntologyControllerAdvice(
            @Autowired OWLHTMLKit kit,
            @Autowired OWLOntologiesService ontService
    ) {
        this.kit = kit;
        this.ontService = ontService;
    }

    @ModelAttribute
    public OWLOntology injectParent(
            @RequestParam(required = false) final String ontId,
            @RequestParam(required = false) String transaction,
            Model model) {
        model.addAttribute("editingEnabled", true);
        model.addAttribute("transaction", transaction);
        return getDefaultOntology(ontId, transaction);
    }

    private OWLOntology getDefaultOntology(String ontId, String transaction) {
        if (transaction != null) {
            IRI ontIRI = TransactionUtils.iriForTransaction(transaction);
            // TODO if the transaction is not known, we should redirect to strip the param out
            return kit.getOntologyForIRI(ontIRI)
                    .orElse(kit.getRootOntology());
        }
        if (ontId != null) {
            return ontService.getOntologyFor(ontId, kit);
        }
        return kit.getRootOntology();
    }
}
