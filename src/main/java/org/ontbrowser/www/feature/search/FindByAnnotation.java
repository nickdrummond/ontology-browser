package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import javax.annotation.Nonnull;
import java.util.*;

@Service
public class FindByAnnotation {

    private final BackendContext backend;

    public FindByAnnotation(BackendContext backend) {
        this.backend = backend;
    }

    public List<AxiomWithMetadata> findByAnnotation(
            @Nonnull String value,
            OWLAnnotationProperty searchProp
    ) {
        Optional<OWLAnnotationProperty> prop = Optional.ofNullable(searchProp);
        Set<AxiomWithMetadata> results = new HashSet<>();
        // TODO this could be a lot more efficient if we delegate to the backend
        for (OWLOntology ont : backend.getRootOntology().getImportsClosure()) {
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
