package org.ontbrowser.www.controller;

import org.apache.http.HttpHeaders;
import org.ontbrowser.www.exception.BadRequestException;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.service.EditService;
import org.ontbrowser.www.service.ParserService;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nickdrummond.parsejs.ParseException;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.util.UUID;

import static org.springframework.web.util.UriComponentsBuilder.fromUri;

@RestController
@Profile("editing")
@RequestMapping(value = "/axioms")
public class EditController extends ApplicationController {

    private static final Logger log = LoggerFactory.getLogger(EditController.class);

    private final ParserService parserService;
    private final EditService editService;

    public EditController(
            @Autowired ParserService parserService,
            @Autowired EditService editService
            ) {
        this.parserService = parserService;
        this.editService = editService;
    }

    /**
     * Keeping this as a GET as we want the browser to be able to do this without JS
     */
    @GetMapping(value = "/add")
    public void add(
            @RequestParam String axiom,
            @RequestParam URI ontology,
            @RequestParam(required = false) String transaction,
            HttpServletRequest request,
            HttpServletResponse response
    ) throws NotFoundException, BadRequestException, IOException {


        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker owlEntityChecker = kit.getOWLEntityChecker();

        OWLOntology targetOnt = kit.getOntologyForIRI(IRI.create(ontology))
                .orElseThrow(() -> new NotFoundException("Target ontology not found: " + ontology));

        OWLAxiom newAxiom;

        try {
            newAxiom = parserService.parseAxiom(axiom, df, owlEntityChecker);
        } catch (ParseException e) {
            throw new BadRequestException("New axiom is not valid Manchester OWL syntax: " + e.getMessage());
        }

        // New transaction
        if (transaction == null || transaction.isBlank()) {
            transaction = UUID.randomUUID().toString();
        }

        OWLOntology transactionOntology =
                editService.add(newAxiom, targetOnt, transaction, kit.getRootOntology());

        String refererHeader = request.getHeader(HttpHeaders.REFERER);
        if (refererHeader != null) {
            URI referer = URI.create(refererHeader);

            response.sendRedirect(fromUri(referer)
                    .replaceQueryParam("transaction", transaction)
                    .build().toString());
        }
        else {
            log.warn("No referer - defaulting to transaction ontology page");
            response.sendRedirect(kit.getURLScheme().getURLForOWLObject(transactionOntology));
        }
    }

    /**
     * Keeping this as a GET as we want the browser to be able to do this without JS
     */
    @GetMapping(value = "/edit")
    public void edit(
        @RequestParam String originalAxiom,
        @RequestParam URI originalOntology,
        @RequestParam String axiom,
        @RequestParam URI ontology,
        @RequestParam(required = false) String transaction,
        HttpServletRequest request,
        HttpServletResponse response
    ) throws NotFoundException, BadRequestException, IOException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker owlEntityChecker = kit.getOWLEntityChecker();

        OWLOntology originalOnt = kit.getOntologyForIRI(IRI.create(originalOntology)).orElseThrow(() -> new NotFoundException("Original ontology not found: " + originalOntology));
        OWLOntology targetOnt = kit.getOntologyForIRI(IRI.create(ontology))
                .orElseThrow(() -> new NotFoundException("Target ontology not found: " + ontology));

        OWLAxiom originalAx;
        OWLAxiom newAxiom;

        try {
            originalAx = parserService.parseAxiom(originalAxiom, df, owlEntityChecker);
        } catch (ParseException e) {
            throw new BadRequestException("Original axiom is not valid Manchester OWL syntax: " + e.getMessage());
        }

        try {
            newAxiom = parserService.parseAxiom(axiom, df, owlEntityChecker);
        } catch (ParseException e) {
            throw new BadRequestException("New axiom is not valid Manchester OWL syntax: " + e.getMessage());
        }

        // New transaction
        if (transaction == null || transaction.isBlank()) {
            transaction = UUID.randomUUID().toString();
        }

        editService.edit(originalAx, newAxiom, originalOnt, targetOnt, transaction, kit.getRootOntology());

        URI referer = URI.create(request.getHeader("referer"));

        response.sendRedirect(fromUri(referer)
                .replaceQueryParam("transaction", transaction)
                .build().toString());
    }
}
