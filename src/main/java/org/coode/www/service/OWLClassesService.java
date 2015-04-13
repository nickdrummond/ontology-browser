package org.coode.www.service;

import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Characteristic;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.InferredDataPropertyCharacteristicAxiomGenerator;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLClassesService {

    // TODO need to index the entities by ID
    public OWLClass getOWLClassFor(final String classId, final OWLHTMLKit kit) throws NotFoundException {
        OWLClass owlThing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();
        if (getIdFor(owlThing).equals(classId)) {
            return owlThing;
        }
        OWLClass owlNothing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLNothing();
        if (getIdFor(owlNothing).equals(classId)) {
            return owlNothing;
        }

        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLClass owlClass: ont.getClassesInSignature()) {
                if (getIdFor(owlClass).equals(classId)){
                    return owlClass;
                }
            }
        }
        throw new NotFoundException("OWLClass", classId);
    }

    public String getIdFor(final OWLClass owlClass) {
        return String.valueOf(owlClass.getIRI().hashCode());
    }

    public HierarchyProvider<OWLClass> getHierarchyProvider(final OWLHTMLKit kit) {
        return kit.getOWLServer().getHierarchyProvider(OWLClass.class);
    }

    public List<Characteristic> getCharacteristics(final OWLClass owlClass, final OWLHTMLKit kit) {
        List<Characteristic> characteristics = new ArrayList<>();
        List<OWLAnnotation> annots = new ArrayList<>();
        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            annots.addAll(EntitySearcher.getAnnotations(owlClass.getIRI(), ont));
        }
        Characteristic annotations = new Characteristic(owlClass, "Annotations", annots);
        characteristics.add(annotations);

        return characteristics;
    }
}
