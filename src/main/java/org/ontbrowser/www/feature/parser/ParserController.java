package org.ontbrowser.www.feature.parser;

import org.ontbrowser.www.backend.BackendContext;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value= ParserController.PATH)
public class ParserController {

    public static final String PATH = "/parse";

    private final BackendContext backend;
    private ParserService parserService;

    public ParserController(
            BackendContext backend,
            ParserService parserService
    ) {
        this.backend = backend;
        this.parserService = parserService;
    }

    @GetMapping(value = "/class-expression")
    public ParseResultJson parseOWLClassExpression(@RequestParam String expression) {
        var df = backend.getOWLDataFactory();
        var checker = backend.getOWLEntityChecker();
        return parserService.parse(expression, df, checker);
    }

    @GetMapping(value = "/axiom")
    public ParseResultJson parseOWLAxiom(@RequestParam String expression) {

        var df = backend.getOWLDataFactory();
        var checker = backend.getOWLEntityChecker();
        return parserService.parseAxiomResult(expression, df, checker);
    }
}
