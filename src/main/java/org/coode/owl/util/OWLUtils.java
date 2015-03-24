package org.coode.owl.util;

import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 29, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class OWLUtils {

    public static Set<OWLOntology> getImportRoots(Set<OWLOntology> onts){
        // TODO: handle cyclic imports
        Set<OWLOntology> roots = new HashSet<OWLOntology>(onts);
        for (OWLOntology ont : onts){
            roots.removeAll(ont.getImports());
        }
        return roots;
    }

    public static String getOntologyIdString(OWLOntology ont){
        if (ont == null){
            System.out.println("ont = " + ont);
        }
        if (ont.isAnonymous()){
            return ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString();
        }
        else{
            return ont.getOntologyID().getDefaultDocumentIRI().toString();
        }
    }

    public static boolean isDeprecated(OWLEntity entity, Set<OWLOntology> ontologies) {
        return false; // there is currently no implementation of deprecated in the OWL API
    }


    public static Set<? extends OWLEntity> getOWLEntitiesFromOntology(NamedObjectType type, OWLOntology ont) {
        switch(type){
            case classes:
                Set<OWLClass> clses = new HashSet<OWLClass>(ont.getClassesInSignature());
                clses.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
                clses.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLNothing());
                return clses;
            case objectproperties:
                Set<OWLObjectProperty> ops = new HashSet<OWLObjectProperty>(ont.getObjectPropertiesInSignature());
                ops.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty());
                ops.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLBottomObjectProperty());
                return ops;
            case dataproperties:
                Set<OWLDataProperty> dps = new HashSet<OWLDataProperty>(ont.getDataPropertiesInSignature());
                dps.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLTopDataProperty());
                dps.add(ont.getOWLOntologyManager().getOWLDataFactory().getOWLBottomDataProperty());
                return dps;
            case annotationproperties: return ont.getAnnotationPropertiesInSignature();
            case individuals: return ont.getIndividualsInSignature();
            case datatypes:
                Set<OWLDatatype> dts = new HashSet<OWLDatatype>(ont.getDatatypesInSignature());
                dts.add(ont.getOWLOntologyManager().getOWLDataFactory().getTopDatatype());
                return dts;
            case entities: return ont.getSignature();
            default: throw new RuntimeException("Object type not known: " + type);
        }
    }

    public static boolean entitiesExist(NamedObjectType type, Set<OWLOntology> onts) {
        for (OWLOntology ont : onts){
            switch (type){
                case classes: if (!ont.getClassesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case objectproperties: if (!ont.getObjectPropertiesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case dataproperties: if (!ont.getDataPropertiesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case individuals: if (!ont.getIndividualsInSignature().isEmpty()){
                    return true;
                }
                    break;
                case annotationproperties: if (!ont.getAnnotationPropertiesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case datatypes: if (!ont.getDatatypesInSignature().isEmpty()){
                    return true;
                }
                    break;
                case entities: return true;
                case ontologies: return true;
            }
            // TODO: no more efficient way to ask this?
        }
        return false;
    }

    public static Map<OWLAnnotationProperty, Set<OWLAnnotationValue>> getAnnotationPropertyMap(OWLNamedIndividual individual, Set<OWLOntology> onts) {
        Map<OWLAnnotationProperty, Set<OWLAnnotationValue>> props = new HashMap<OWLAnnotationProperty, Set<OWLAnnotationValue>>();
        for (OWLOntology ont : onts){
            for (OWLAnnotationAssertionAxiom ax : individual.getAnnotationAssertionAxioms(ont)){
                OWLAnnotationProperty p = ax.getProperty();
                Set<OWLAnnotationValue> objects = props.get(p);
                if (objects == null){
                    objects = new HashSet<OWLAnnotationValue>();
                    props.put(p, objects);
                }
                objects.add(ax.getAnnotation().getValue());
            }
        }
        return props;
    }

    public static Map<OWLPropertyExpression, Set<OWLPropertyAssertionObject>> getAssertedPropertyMap(OWLNamedIndividual individual, Set<OWLOntology> onts) {
        Map<OWLPropertyExpression, Set<OWLPropertyAssertionObject>> props = new HashMap<OWLPropertyExpression, Set<OWLPropertyAssertionObject>>();
        for (OWLOntology ont : onts){
            for (OWLAxiom ax : individual.getReferencingAxioms(ont)){
                if (ax instanceof OWLPropertyAssertionAxiom){
                    OWLPropertyAssertionAxiom propAssertion = (OWLPropertyAssertionAxiom)ax;
                    if (propAssertion.getSubject().equals(individual)){
                        OWLPropertyExpression p = propAssertion.getProperty();
                        Set<OWLPropertyAssertionObject> objects = props.get(p);
                        if (objects == null){
                            objects = new HashSet<OWLPropertyAssertionObject>();
                            props.put(p, objects);
                        }
                        objects.add(propAssertion.getObject());
                    }
                }
            }
        }
        return props;
    }
//
//    public static Map<OWLPropertyExpression, Set<OWLPropertyAssertionObject>> getInferredPropertyMap(OWLNamedIndividual individual, OWLReasoner r) {
//        Map<OWLPropertyExpression, Set<OWLPropertyAssertionObject>> props = new HashMap<OWLPropertyExpression, Set<OWLPropertyAssertionObject>>();
//
//        for (OWLIndividual same : r.getSameIndividuals(individual)){
//            getAssertedPropertyMap()
//        }
//
//        for (OWLOntology ont : onts){
//            for (OWLAxiom ax : individual.getReferencingAxioms(ont)){
//                if (ax instanceof OWLPropertyAssertionAxiom){
//                    OWLPropertyAssertionAxiom propAssertion = (OWLPropertyAssertionAxiom)ax;
//                    if (propAssertion.getSubject().equals(individual)){
//                        OWLPropertyExpression p = propAssertion.getProperty();
//                        Set<OWLPropertyAssertionObject> objects = props.get(p);
//                        if (objects == null){
//                            objects = new HashSet<OWLPropertyAssertionObject>();
//                            props.put(p, objects);
//                        }
//                        objects.add(propAssertion.getObject());
//                    }
//                }
//            }
//        }
//        return props;
//    }

    public static OWLNamedIndividual getIndividual(IRI iri, Set<OWLOntology> onts) {
        for (OWLOntology ont : onts){
            if (ont.containsIndividualInSignature(iri)){
                return ont.getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(iri);
            }
        }
        return null;
    }

    public static boolean isStructural(OWLReasoner r) {
        return r.getReasonerName().equals("Structural Reasoner");
    }
}
