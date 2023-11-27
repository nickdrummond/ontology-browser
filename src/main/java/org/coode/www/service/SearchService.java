package org.coode.www.service;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Convert a request to a regex which can be found by the OWLEntityFinder.
 */
@Service
public class SearchService {

    private final String wildcard = "*";

    public List<OWLEntity> findByName(final String input, final OWLHTMLKit kit) {

        if ((input == null) || (input.isEmpty())) {
            return Collections.emptyList();
        }

        String searchStr = (input.endsWith(wildcard)) ? ".*" + input.replace("\\" + wildcard, ".*") :
                ".*" + input + ".*";

        // Sort first by index of search and then alphabetically
        Comparator<OWLEntity> c = new Comparator<>() {
            final ShortFormProvider sfp = kit.getShortFormProvider();
            final String str = input.replaceAll("\\" + wildcard, "").toLowerCase();

            @Override
            public int compare(OWLEntity o1, OWLEntity o2) {
                String sf1 = sfp.getShortForm(o1).toLowerCase();
                String sf2 = sfp.getShortForm(o2).toLowerCase();
                int i1 = sf1.indexOf(str);
                int i2 = sf2.indexOf(str);
                if (i1 == i2) {
                    return sf1.compareTo(sf2);
                } else if (i1 < i2) {
                    return -1;
                }
                return 1;
            }
        };

        return kit.getFinder().getOWLEntities(searchStr).stream().sorted(c).collect(Collectors.toList());
    }

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

    public Set<OWLEntity> getEntities(IRI iri, OWLHTMLKit kit) {
        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLOntology activeOnt = kit.getActiveOntology();
        return EntityType.values().stream()
                .map(t -> {
                    OWLEntity e = df.getOWLEntity(t, iri);
                    if (activeOnt.containsEntityInSignature(e, Imports.INCLUDED)) {
                        return e;
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
    }

}
