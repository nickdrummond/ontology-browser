package org.coode.www.service;

import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class OWLAxiomService {

    private final Map<OWLAxiom, String> axiomRenderCache = new HashMap<>();

    public Set<OWLAxiom> findAxioms(final String search, final OWLOntology ont, final ShortFormProvider sfp) {
        if (axiomRenderCache.isEmpty()) {
            ont.getAxioms(Imports.INCLUDED).forEach(ax -> axiomRenderCache.put(ax, render(ax, sfp)));
        }

        if (search == null || search.isEmpty()) {
            return axiomRenderCache.keySet();
        }
        else {
            return axiomRenderCache.entrySet().stream()
                    .filter(e -> e.getValue().contains(search))
                    .map(Map.Entry::getKey)
                    .collect(Collectors.toSet());
        }
    }


    public Set<OWLAxiom> regexAxioms(final String search, final OWLOntology ont, final ShortFormProvider sfp) {
        if (axiomRenderCache.isEmpty()) {
            ont.getAxioms(Imports.INCLUDED).forEach(ax -> axiomRenderCache.put(ax, render(ax, sfp)));
        }

        if (search == null || search.isEmpty()) {
            return axiomRenderCache.keySet();
        }
        else {
            return axiomRenderCache.entrySet().stream()
                    .filter(e -> e.getValue().matches(search))
                    .map(Map.Entry::getKey)
                    .collect(Collectors.toSet());
        }
    }

    private String render(final OWLAxiom axiom, final ShortFormProvider sfp) {
        StringWriter writer = new StringWriter();
        ManchesterOWLSyntaxObjectRenderer mosRenderer = new ManchesterOWLSyntaxObjectRenderer(writer, sfp);
        axiom.accept(mosRenderer);
        return writer.toString();
    }

}
