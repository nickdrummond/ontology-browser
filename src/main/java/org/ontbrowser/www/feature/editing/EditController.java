package org.ontbrowser.www.feature.editing;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.http.HttpHeaders;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;
import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;

import java.io.IOException;
import java.net.URI;
import java.util.UUID;

import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.springframework.http.HttpStatus.NOT_FOUND;
import static org.springframework.web.util.UriComponentsBuilder.fromUri;

@RestController
@Profile("editing")
@RequestMapping(value = "/axioms")
public class EditController extends ApplicationController {

    private static final Logger log = LoggerFactory.getLogger(EditController.class);

    private final EditService editService;

    public EditController(EditService editService) {
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
    ) throws IOException {


        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker owlEntityChecker = kit.getOWLEntityChecker();

        OWLOntology targetOnt = kit.getOntologyForIRI(IRI.create(ontology))
                .orElseThrow(() -> new ResponseStatusException(NOT_FOUND, "Target ontology not found: " + ontology));

        OWLAxiom newAxiom;

        try {
            newAxiom = editService.parseAxiom(axiom, df, owlEntityChecker);
        } catch (ParseException e) {
            throw new ResponseStatusException(BAD_REQUEST, "New axiom is not valid Manchester OWL syntax: " + e.getMessage());
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
        } else {
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
    ) throws IOException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker owlEntityChecker = kit.getOWLEntityChecker();

        OWLOntology originalOnt = kit.getOntologyForIRI(IRI.create(originalOntology)).orElseThrow(() -> new ResponseStatusException(NOT_FOUND, "Original ontology not found: " + originalOntology));
        OWLOntology targetOnt = kit.getOntologyForIRI(IRI.create(ontology))
                .orElseThrow(() -> new ResponseStatusException(NOT_FOUND, "Target ontology not found: " + ontology));

        OWLAxiom originalAx;
        OWLAxiom newAxiom;

        try {
            originalAx = editService.parseAxiom(originalAxiom, df, owlEntityChecker);
        } catch (ParseException e) {
            throw new ResponseStatusException(BAD_REQUEST, "Original axiom is not valid Manchester OWL syntax: " + e.getMessage());
        }

        try {
            newAxiom = editService.parseAxiom(axiom, df, owlEntityChecker);
        } catch (ParseException e) {
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


    @GetMapping(value = "/ac", produces = MediaType.APPLICATION_XML_VALUE)
    public String autocompleteOWLAxiom(
            @RequestParam String expression) {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();
        OWLEntityFinder finder = kit.getFinder();
        ShortFormProvider sfp = kit.getShortFormProvider();

        return editService.autocompleteAxiom(expression, df, checker, finder, sfp).toString();
    }

    @GetMapping(value = "/parse", produces = MediaType.APPLICATION_XML_VALUE)
    public String parseOWLAxiom(
            @RequestParam String expression) {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();

        try {
            // TODO this needs to be tidier - return the Axiom and let the OK be the response status
            return new ParseResult(kit.getStringRenderer().render(editService.parseAxiom(expression, df, checker)), "OK").toString();
        } catch (ParseException e) {
            return e.toString();
        }
    }

}
