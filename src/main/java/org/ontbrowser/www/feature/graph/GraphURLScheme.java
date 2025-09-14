package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.*;

public class GraphURLScheme implements URLScheme {

    public static final String ROOT_PATH = "/graph";
    private final MOSStringRenderer mos;

    public GraphURLScheme(MOSStringRenderer mos) {
        this.mos = mos;
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject, OWLOntology ontology) {
        return owlObject.accept(new OWLObjectVisitorEx<>() {
            @Override
            public String visit(OWLNamedIndividual individual) {
                return ROOT_PATH + "?indivs=" + mos.render(individual);
            }

            @Override
            public String visit(OWLClass cls) {
                return ROOT_PATH + "?depth=0&query=" + mos.render(cls);
            }

            @Override
            public String visit(OWLObjectProperty property) {
                return ROOT_PATH + "?props=" + mos.render(property);
            }

            @Override
            public String visit(OWLDataProperty property) {
                return ROOT_PATH + "?props=" + mos.render(property);
            }

            @Override
            public <T> String doDefault(T object) {
                if (object instanceof OWLClassExpression expr) {
                    return ROOT_PATH + "?depth=0&query=" + mos.render(expr);
                }
                return OWLObjectVisitorEx.super.doDefault(object);
            }
        });
    }
}
