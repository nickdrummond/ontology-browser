package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import javax.annotation.Nonnull;
import java.util.*;

@Service
public class FindByAnnotation {

    public List<AxiomWithMetadata> findByAnnotation(@Nonnull String value,
                                                    OWLAnnotationProperty searchProp,
                                                    @Nonnull OWLHTMLKit kit) {
        Optional<OWLAnnotationProperty> prop = Optional.ofNullable(searchProp);
        Set<AxiomWithMetadata> results = new HashSet<>();
        for (OWLOntology ont : kit.getOWLOntologyManager().getOntologies()) {
            for (OWLAnnotationAssertionAxiom ax : ont.getAxioms(AxiomType.ANNOTATION_ASSERTION)) {
                if (prop.isEmpty() || prop.get().equals(ax.getProperty())) {
                    OWLAnnotationValue v = ax.getValue();
                    if (v.isLiteral() && v.asLiteral().isPresent() && v.asLiteral().get().getLiteral().contains(value)) {
                        results.add(new AxiomWithMetadata("results", ax, ax, ont));
                    }
                }
            }
        }
        return new ArrayList<>(results);
    }
}
