package org.coode.www.service;

import com.google.common.base.Optional;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Set;

@Service
public class GeoService {

    // TODO IRIs should be created once
    @Value("${geo.latitude}")
    private String latitude;

    @Value("${geo.longitude}")
    private String longitude;

    @Value("${geo.point}")
    private String pointr;

    public Optional<Loc> getLocation(final OWLEntity owlEntity, final Set<OWLOntology> onts) {
        if (onts == null || onts.isEmpty()){
            throw new IllegalArgumentException("Ontologies cannot be empty");
        }

        if (owlEntity.isOWLNamedIndividual()){
            OWLNamedIndividual owlNamedIndividual = owlEntity.asOWLNamedIndividual();

            Loc loc = new Loc();
            for (OWLOntology ont : onts) {
                for (OWLAnnotationAssertionAxiom axiom : ont.getAnnotationAssertionAxioms(owlNamedIndividual.getIRI())) {
                    final IRI iri = axiom.getProperty().getIRI();

                    if (iri.equals(IRI.create(pointr))) {
                        Optional<OWLLiteral> maybeLiteral = axiom.getValue().asLiteral();
                        if (maybeLiteral.isPresent()) {
                            String[] latLong = maybeLiteral.get().getLiteral().trim().split("\\s+");
                            if (latLong.length == 2) {
                                loc.latitude = latLong[0];
                                loc.longitude = latLong[1];
                            }
                        }
                    } else if (iri.equals(IRI.create(latitude))) {
                        Optional<OWLLiteral> maybeLiteral = axiom.getValue().asLiteral();
                        if (maybeLiteral.isPresent()) {
                            loc.latitude = maybeLiteral.get().getLiteral().trim();
                        }
                    } else if (iri.equals(IRI.create(longitude))) {
                        Optional<OWLLiteral> maybeLiteral = axiom.getValue().asLiteral();
                        if (maybeLiteral.isPresent()) {
                            loc.longitude = maybeLiteral.get().getLiteral().trim();
                        }
                    }

                    if (loc.latitude != null && loc.longitude != null) {
                        return Optional.fromNullable(loc);
                    }
                }
            }
        }
        return Optional.absent();
    }

    public static class Loc {
        public String latitude;
        public String longitude;
    }
}
