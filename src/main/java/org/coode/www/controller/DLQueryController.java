package org.coode.www.controller;

import java.util.Optional;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.QueryType;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.ParserService;
import org.coode.www.service.ReasonerService;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import uk.co.nickdrummond.parsejs.ParseException;

import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Controller
@RequestMapping(value="/dlquery")
@SessionAttributes("kit")
public class DLQueryController extends ApplicationController {

    @Autowired
    private ParserService service;

    @Autowired
    private ReasonerService reasonerService;

    @RequestMapping(method=RequestMethod.GET)
    public String dlQuery(
            @RequestParam(required = false, defaultValue = "") final String expression,
            @ModelAttribute("kit") final OWLHTMLKit kit,
            final Model model) throws OntServerException, ParseException {

        model.addAttribute("options", optionsService.getConfig(kit));
        model.addAttribute("activeOntology", kit.getActiveOntology());
        model.addAttribute("ontologies", kit.getOntologies());
        model.addAttribute("expression", expression);

        return "dlquery";
    }

    @RequestMapping(value="results",method=RequestMethod.GET)
    public String getResults(
            @RequestParam(required = true) final String expression,
            @RequestParam(required = true) final QueryType query,
            @ModelAttribute("kit") final OWLHTMLKit kit,
            HttpServletResponse response,
            final Model model) throws OntServerException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();
        OWLReasoner reasoner = kit.getOWLReasoner();
        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.empty());

        try {
            OWLClassExpression owlClassExpression = service.getOWLClassExpression(expression, df, checker);
            List<OWLEntity> results = new ArrayList<>(reasonerService.getResults(owlClassExpression, query, reasoner));

            logger.debug("Results count: " + results.size());

            Collections.sort(results, kit.getComparator());
            Characteristic resultsCharacteristic = new Characteristic(null, query.name(), results);

            model.addAttribute("results", resultsCharacteristic);
            model.addAttribute("mos", owlRenderer);

            return "base :: results";
        } catch (ParserException e) {
            response.setStatus(HttpStatus.BAD_REQUEST.value());
            return "Bad OWLClassExpression: " + expression;
        }
    }

    @RequestMapping(value = "/ac", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String autocompleteOWLClassExpression(
            @RequestParam(required = true) String expression,
            @ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();
        OWLEntityFinder finder = kit.getFinder();
        ShortFormProvider sfp = kit.getShortFormProvider();

        return service.autocomplete(expression, df, checker, finder, sfp).toString();
    }

    // TODO return the actual ParseResult or an XML rendering of the parse exception
    @RequestMapping(value = "/parse", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String parseOWLClassExpression(
            @RequestParam(required = true) String expression,
            @ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();

        try {
            return service.parse(expression, df, checker).toString();
        } catch (ParseException e) {
            return e.toString();
        }
    }
}
