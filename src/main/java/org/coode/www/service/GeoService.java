package org.coode.www.service;

import com.google.common.base.Optional;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import java.util.Set;

@Service
public class GeoService {

    private IRI latitude;
    private IRI longitude;
    private IRI pointr;

    public GeoService(String latitude, String longitude, String pointr) {
        this.latitude = IRI.create(latitude);
        this.longitude = IRI.create(longitude);
        this.pointr = IRI.create(pointr);
    }

    public Optional<Loc> getLocation(final OWLEntity owlEntity, final Set<OWLOntology> onts) {
        if (onts == null || onts.isEmpty()){
            throw new IllegalArgumentException("Ontologies cannot be empty");
        }

        if (owlEntity.isOWLNamedIndividual()){
            OWLNamedIndividual owlNamedIndividual = owlEntity.asOWLNamedIndividual();

            Loc loc = new Loc();
            for (OWLOntology ont : onts) {
                for (OWLAnnotationAssertionAxiom axiom : ont.getAnnotationAssertionAxioms(owlNamedIndividual.getIRI())) {
                    if (axiom.getProperty().getIRI().equals(pointr)) {
                        Optional<OWLLiteral> maybeLiteral = axiom.getValue().asLiteral();
                        if (maybeLiteral.isPresent()) {
                            String[] latLong = maybeLiteral.get().getLiteral().trim().split("\\s+");
                            if (latLong.length == 2) {
                                loc.latitude = latLong[0];
                                loc.longitude = latLong[1];
                            }
                        }
                    } else if (axiom.getProperty().getIRI().equals(latitude)) {
                        Optional<OWLLiteral> maybeLiteral = axiom.getValue().asLiteral();
                        if (maybeLiteral.isPresent()) {
                            loc.latitude = maybeLiteral.get().getLiteral().trim();
                        }
                    } else if (axiom.getProperty().getIRI().equals(longitude)) {
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
