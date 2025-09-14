package org.ontbrowser.www.feature.parser;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value= AutocompleteController.PATH)
public class AutocompleteController extends ApplicationController {

    public static final String PATH = "/autocomplete";

    private AutocompleteService autocompleteService;

    public AutocompleteController(AutocompleteService autocompleteService) {
        this.autocompleteService = autocompleteService;
    }

    @GetMapping(value = "/class-expression")
    public AutocompleteResultJson autocompleteOWLClassExpression(@RequestParam String expression) {
        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var checker = kit.getOWLEntityChecker();
        var finder = kit.getFinder();
        var sfp = kit.getShortFormProvider();
        return autocompleteService.autocomplete(expression, df, checker, finder, sfp);
    }

    @GetMapping(value = "/axiom")
    public AutocompleteResultJson autocompleteOWLAxiom(
            @RequestParam String expression) {

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();
        OWLEntityFinder finder = kit.getFinder();
        ShortFormProvider sfp = kit.getShortFormProvider();

        return autocompleteService.autocompleteAxiom(expression, df, checker, finder, sfp);
    }

    @GetMapping(value = "/individuals")
    public AutocompleteResultJson autocompleteIndividualsList(@RequestParam String expression) {
        var finder = kit.getFinder();
        var sfp = kit.getShortFormProvider();
        return autocompleteService.autocompleteIndividualsList(expression, finder, sfp);
    }

    // Object and data properties
    @GetMapping(value = "/properties")
    public AutocompleteResultJson autocompletePropertiesList(@RequestParam String expression) {
        var finder = kit.getFinder();
        var sfp = kit.getShortFormProvider();
        return autocompleteService.autocompletePropertiesList(expression, finder, sfp);
    }
}
