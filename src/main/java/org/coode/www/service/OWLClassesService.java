package org.coode.www.service;

import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Characteristic;
import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.*;
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

        Set<OWLOntology> activeOntologies = kit.getOWLServer().getActiveOntologies();
        Comparator<OWLObject> comparator = kit.getOWLServer().getComparator();

        List<Characteristic> characteristics = new ArrayList<>();

        List<OWLAnnotation> annots = new ArrayList<>();
        for (OWLOntology ont : activeOntologies){
            annots.addAll(EntitySearcher.getAnnotations(owlClass.getIRI(), ont));
        }
        if (!annots.isEmpty()) {
            Characteristic annotations = new Characteristic(owlClass, "Annotations", annots);
            characteristics.add(annotations);
        }

        List<OWLClassExpression> equivs = new ArrayList<>(EntitySearcher.getEquivalentClasses(owlClass, activeOntologies));
        if (!equivs.isEmpty()) {
            Collections.sort(equivs, comparator);
            Characteristic equivalents = new Characteristic(owlClass, "Equivalents", equivs);
            characteristics.add(equivalents);
        }

//        addDoclet(new OWLEntityTitleDoclet<OWLClass>(kit));
//        addDoclet(new AnnotationsDoclet<OWLClass>(kit));
//        addDoclet(new AssertedEquivalentsDoclet(kit));
//        addDoclet(new SuperclassesDoclet(kit));
//        addDoclet(new DisjointsDoclet(kit));
//        addDoclet(new MembersDoclet(kit));

        UsageVisibilityVisitor usageVisibilityVisitor = new UsageVisibilityVisitor();
        List<OWLObject> usage = new ArrayList<>();
        for (OWLOntology ont : activeOntologies){
            for (OWLAxiom ax : ont.getReferencingAxioms(owlClass)){
                if (usageVisibilityVisitor.getShowUsage(ax, owlClass)){
                    usage.add(ax);
                }
            }
        }
        if (!usage.isEmpty()) {
            Collections.sort(usage, comparator);
            Characteristic usageC = new Characteristic(owlClass, "Usage", usage);
            characteristics.add(usageC);
        }

        return characteristics;
    }
}
