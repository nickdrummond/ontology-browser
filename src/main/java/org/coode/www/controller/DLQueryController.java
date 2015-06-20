package org.coode.www.controller;

import org.coode.html.doclet.ReasonerResultsDoclet;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.QueryType;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.ParserService;
import org.coode.www.service.ReasonerService;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import uk.co.nickdrummond.parsejs.ParseException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Set;

@Controller
@RequestMapping(value="/dlquery")
public class DLQueryController extends ApplicationController {

    @Autowired
    private ParserService service;

    @Autowired
    private ReasonerService reasonerService;

    @RequestMapping(method=RequestMethod.GET)
    public String dlQuery(
            @RequestParam(required = false, defaultValue = "") final String expression,
            @RequestParam(required = false) final String label,
            HttpServletRequest request,
            Model model) throws OntServerException, ParseException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("expression", expression);

        return "dlquery";
    }

    @RequestMapping(value="results",method=RequestMethod.GET)
    public @ResponseBody String getResults(
            @RequestParam(required = true) final String expression,
            @RequestParam(required = true) final QueryType query,
            @RequestParam(required = false) final String label,
            HttpServletRequest request,
            HttpServletResponse response) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLServer owlServer = kit.getOWLServer();
        OWLDataFactory df = owlServer.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = owlServer.getOWLEntityChecker();

        try {
            OWLClassExpression owlClassExpression = service.getOWLClassExpression(expression, df, checker);
            Set<OWLEntity> results = reasonerService.getResults(owlClassExpression, query, kit.getOWLServer().getOWLReasoner());
            ReasonerResultsDoclet resultsDoclet = new ReasonerResultsDoclet(query, results, kit);
            return renderDoclets(request, resultsDoclet);
        } catch (ParserException e) {
            response.setStatus(HttpStatus.BAD_REQUEST.value());
            return "Bad OWLClassExpression: " + expression;
        }
    }

    @RequestMapping(value = "/ac", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String autocompleteOWLClassExpression(
            @RequestParam(required = true) String expression,
            @RequestParam(required = false) final String label,
            HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLServer owlServer = kit.getOWLServer();
        OWLDataFactory df = owlServer.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = owlServer.getOWLEntityChecker();
        OWLEntityFinder finder = owlServer.getFinder();
        ShortFormProvider sfp = owlServer.getShortFormProvider();

        return service.autocomplete(expression, df, checker, finder, sfp).toString();
    }

    // TODO return the actual ParseResult or an XML rendering of the parse exception
    @RequestMapping(value = "/parse", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody String parseOWLClassExpression(
            @RequestParam(required = true) String expression,
            @RequestParam(required = false) final String label,
            HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        OWLServer owlServer = kit.getOWLServer();
        OWLDataFactory df = owlServer.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = owlServer.getOWLEntityChecker();

        try {
            return service.parse(expression, df, checker).toString();
        } catch (ParseException e) {
            return e.toString();
        }
    }
}
