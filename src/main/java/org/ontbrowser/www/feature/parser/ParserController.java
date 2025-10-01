package org.ontbrowser.www.feature.parser;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value= ParserController.PATH)
public class ParserController {

    public static final String PATH = "/parse";

    private final OWLHTMLKit kit;
    private ParserService parserService;

    public ParserController(
            OWLHTMLKit kit,
            ParserService parserService
    ) {
        this.kit = kit;
        this.parserService = parserService;
    }

    @GetMapping(value = "/class-expression")
    public ParseResultJson parseOWLClassExpression(@RequestParam String expression) {
        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var checker = kit.getOWLEntityChecker();
        return parserService.parse(expression, df, checker);
    }

    @GetMapping(value = "/axiom")
    public ParseResultJson parseOWLAxiom(@RequestParam String expression) {

        var df = kit.getOWLOntologyManager().getOWLDataFactory();
        var checker = kit.getOWLEntityChecker();
        return parserService.parseAxiomResult(expression, df, checker);
    }
}
