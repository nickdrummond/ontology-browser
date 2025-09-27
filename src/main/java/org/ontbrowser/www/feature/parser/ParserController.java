package org.ontbrowser.www.feature.parser;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value= ParserController.PATH)
public class ParserController  extends ApplicationController {

    public static final String PATH = "/parse";

    private ParserService parserService;

    public ParserController(
            RestartableKit kit,
            ParserService parserService
    ) {
        super(kit);
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

        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLEntityChecker checker = kit.getOWLEntityChecker();
        return parserService.parseAxiomResult(expression, df, checker);
    }
}
