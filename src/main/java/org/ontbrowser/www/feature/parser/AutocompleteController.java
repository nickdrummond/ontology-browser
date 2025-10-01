package org.ontbrowser.www.feature.parser;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value= AutocompleteController.PATH)
public class AutocompleteController {

    public static final String PATH = "/autocomplete";

    private final OWLHTMLKit kit;
    private AutocompleteService autocompleteService;

    public AutocompleteController(
            OWLHTMLKit kit,
            AutocompleteService autocompleteService
    ) {
        this.kit = kit;
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
    public AutocompleteResultJson autocompleteOWLAxiom(@RequestParam String expression) {
        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var checker = kit.getOWLEntityChecker();
        var finder = kit.getFinder();
        var sfp = kit.getShortFormProvider();

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
