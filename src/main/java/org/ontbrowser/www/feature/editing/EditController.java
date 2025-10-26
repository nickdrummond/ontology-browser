package org.ontbrowser.www.feature.editing;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.http.HttpHeaders;
import org.ontbrowser.www.feature.parser.ParserService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;

import java.io.IOException;
import java.net.URI;
import java.util.UUID;

import static org.springframework.http.HttpStatus.*;
import static org.springframework.web.util.UriComponentsBuilder.fromUri;

@RestController
@Profile("editing")
@RequestMapping(value = "/axioms")
@PreAuthorize("hasRole('ADMIN')")
public class EditController {

    private static final Logger log = LoggerFactory.getLogger(EditController.class);

    private final OWLHTMLKit kit;
    private final EditService editService;
    private final ParserService parserService;

    public EditController(
            OWLHTMLKit kit,
            EditService editService,
            ParserService parserService
    ) {
        this.kit = kit;
        this.editService = editService;
        this.parserService = parserService;
    }

    /**
     * Keeping this as a GET as we want the browser to be able to do this without JS
     */
    @RequiresEditable
    @GetMapping(value = "/add")
    public void add(
            @RequestParam String axiom,
            @RequestParam URI ontology,
            @RequestParam(required = false) String transaction,
            HttpServletRequest request,
            HttpServletResponse response
    ) throws IOException {

        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var owlEntityChecker = kit.getOWLEntityChecker();

        var targetOnt = kit.getOntologyForIRI(IRI.create(ontology))
                .orElseThrow(() -> new ResponseStatusException(NOT_FOUND, "Target ontology not found: " + ontology));

        OWLAxiom newAxiom;

        try {
            newAxiom = parserService.parseAxiom(axiom, df, owlEntityChecker);
        } catch (ParserException e) {
            throw new ResponseStatusException(BAD_REQUEST, "New axiom is not valid Manchester OWL syntax: " + e.getMessage());
        }

        // New transaction
        if (transaction == null || transaction.isBlank()) {
            transaction = UUID.randomUUID().toString();
        }

        var transactionOntology =
                editService.add(newAxiom, targetOnt, transaction, kit.getRootOntology());

        String refererHeader = request.getHeader(HttpHeaders.REFERER);
        if (refererHeader != null) {
            URI referer = URI.create(refererHeader);

            response.sendRedirect(fromUri(referer)
                    .replaceQueryParam("transaction", transaction)
                    .build().toString());
        } else {
            log.warn("No referer - defaulting to transaction ontology page");
            response.sendRedirect(kit.getURLScheme().getURLForOWLObject(transactionOntology, targetOnt));
        }
    }

    /**
     * Keeping this as a GET as we want the browser to be able to do this without JS
     */
    @RequiresEditable
    @GetMapping(value = "/edit")
    public void edit(
            @RequestParam String originalAxiom,
            @RequestParam URI originalOntology,
            @RequestParam String axiom,
            @RequestParam URI ontology,
            @RequestParam(required = false) String transaction,
            HttpServletRequest request,
            HttpServletResponse response
    ) throws IOException {

        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var owlEntityChecker = kit.getOWLEntityChecker();

        var originalOnt = kit.getOntologyForIRI(IRI.create(originalOntology)).orElseThrow(() -> new ResponseStatusException(NOT_FOUND, "Original ontology not found: " + originalOntology));
        var targetOnt = kit.getOntologyForIRI(IRI.create(ontology))
                .orElseThrow(() -> new ResponseStatusException(NOT_FOUND, "Target ontology not found: " + ontology));

        OWLAxiom originalAx;
        OWLAxiom newAxiom;

        try {
            originalAx = parserService.parseAxiom(originalAxiom, df, owlEntityChecker);
        } catch (ParserException e) {
            throw new ResponseStatusException(BAD_REQUEST, "Original axiom is not valid Manchester OWL syntax: " + e.getMessage());
        }

        try {
            newAxiom = parserService.parseAxiom(axiom, df, owlEntityChecker);
        } catch (ParserException e) {
            throw new ResponseStatusException(BAD_REQUEST, "New axiom is not valid Manchester OWL syntax: " + e.getMessage());
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
