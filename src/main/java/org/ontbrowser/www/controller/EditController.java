package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.BadRequestException;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.renderer.ElementRenderer;
import org.ontbrowser.www.service.EditService;
import org.ontbrowser.www.service.OWLAxiomService;
import org.ontbrowser.www.service.OWLOntologiesService;
import org.ontbrowser.www.service.ParserService;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import org.springframework.web.util.UriComponentsBuilder;
import uk.co.nickdrummond.parsejs.ParseException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static org.ontbrowser.www.renderer.HighlightingHTMLRenderer.getHighlightRenderer;
import static org.springframework.web.util.UriComponentsBuilder.fromUri;

@RestController
@RequestMapping(value = "/axioms")
public class EditController extends ApplicationController {

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

        OWLOntology targetOnt = kit.getOntologyForIRI(IRI.create(ontology)).orElseThrow(() -> new NotFoundException("Target ontology not found: " + ontology));

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

        editService.add(newAxiom, targetOnt, transaction, kit.getActiveOntology());

        URI referer = URI.create(request.getHeader("referer"));

        response.sendRedirect(fromUri(referer)
                .replaceQueryParam("transaction", transaction)
                .build().toString());
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
        OWLOntology targetOnt = kit.getOntologyForIRI(IRI.create(ontology)).orElseThrow(() -> new NotFoundException("Target ontology not found: " + ontology));

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

        editService.edit(originalAx, newAxiom, originalOnt, targetOnt, transaction, kit.getActiveOntology());

        URI referer = URI.create(request.getHeader("referer"));

        response.sendRedirect(fromUri(referer)
                .replaceQueryParam("transaction", transaction)
                .build().toString());
    }
}
