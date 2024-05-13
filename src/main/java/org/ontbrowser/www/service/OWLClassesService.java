package org.ontbrowser.www.service;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.characteristics.ClassCharacteristicsBuilder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

import static org.semanticweb.owlapi.model.AxiomType.SUBCLASS_OF;

@Service
public class OWLClassesService {

    public OWLClass getOWLClassFor(final String classId, final OWLOntology ont) throws NotFoundException {
        OWLDataFactory df = ont.getOWLOntologyManager().getOWLDataFactory();

        OWLClass owlThing = df.getOWLThing();
        if (getIdFor(owlThing).equals(classId)) {
            return owlThing;
        }
        OWLClass owlNothing = df.getOWLNothing();
        if (getIdFor(owlNothing).equals(classId)) {
            return owlNothing;
        }

        for (OWLClass owlClass: ont.getClassesInSignature(Imports.INCLUDED)) {
            if (getIdFor(owlClass).equals(classId)){
                return owlClass;
            }
        }
        throw new NotFoundException("OWLClass", classId);
    }

    public String getIdFor(final OWLClass owlClass) {
        return String.valueOf(owlClass.getIRI().hashCode());
    }

    public List<Characteristic> getCharacteristics(
            final OWLClass owlClass,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int defaultPageSize) {
        return new ClassCharacteristicsBuilder(owlClass, ont, comparator, with, defaultPageSize).getCharacteristics();
    }

    public Set<OWLClass> getNamedTypes(OWLClass cls, OWLOntology ont) {
        return ont.getAxioms(cls, Imports.INCLUDED).stream()
                .filter(ax -> ax.isOfType(SUBCLASS_OF))
                .map(ax -> ((OWLSubClassOfAxiom)ax).getSuperClass())
                .filter(OWLClassExpression::isNamed)
                .map(OWLClassExpression::asOWLClass)
                .collect(Collectors.toSet());
    }
}
