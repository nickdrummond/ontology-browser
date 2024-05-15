package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.service.OWLOntologiesService;
import org.ontbrowser.www.util.TransactionUtils;
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
            Model model) throws NotFoundException {
        model.addAttribute("editingEnabled", true);
        model.addAttribute("transaction", transaction);
        return getDefaultOntology(ontId, transaction);
    }

    private OWLOntology getDefaultOntology(String ontId, String transaction) throws NotFoundException {
        if (transaction != null) {
            IRI ontIRI = TransactionUtils.iriForTransaction(transaction);
            return kit.getOntologyForIRI(ontIRI)
                    .orElseThrow(() -> new NotFoundException("No ontology for the given transaction: " + transaction));
        }
        if (ontId != null) {
            return ontService.getOntologyFor(ontId, kit);
        }
        return kit.getActiveOntology();
    }
}
