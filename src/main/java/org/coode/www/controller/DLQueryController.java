package org.coode.www.controller;

import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.ParserService;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import uk.co.nickdrummond.parsejs.AutocompleteResult;
import uk.co.nickdrummond.parsejs.ParseException;
import uk.co.nickdrummond.parsejs.ParseResult;

import javax.servlet.http.HttpServletRequest;

@Controller
@RequestMapping(value="/dlquery")
public class DLQueryController extends ApplicationController {

    @Autowired
    private ParserService service;

    @RequestMapping(method=RequestMethod.GET)
    public String dlQuery(
            @RequestParam(required = false, defaultValue = "") final String expression,
            @RequestParam(required = false) final String label,
            HttpServletRequest request,
            Model model) throws OntServerException, ParseException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        model.addAttribute("applicationInfo", applicationInfo);
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("expression", expression);

        return "dlquery";
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
