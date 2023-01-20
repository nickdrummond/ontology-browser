package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.*;

public enum NamedObjectType {

    ontologies ("Ontologies", "Ontology"),
    entities ("Entities", "Entity"),
    classes ("Classes", "Class"),
    objectproperties ("Object Properties", "Object Property"),
    dataproperties ("Data Properties", "Data Property"),
    annotationproperties ("Annotation Properties", "Annotation Property"),
    individuals ("Individuals", "Individual"),
    datatypes ("Datatypes", "Datatype");

    private static final NamedObjectType[] entitySubTypes =
            new NamedObjectType[]{classes, objectproperties, dataproperties, annotationproperties, individuals, datatypes};

    private final String plural;
    private final String singular;

    NamedObjectType(String plural, String singular){
        this.plural = plural;
        this.singular = singular;
    }

    public String getPluralRendering(){
        return plural;
    }

    public String getSingularRendering() {
        return singular;
    }

    public static NamedObjectType[] entitySubtypes() {
        return entitySubTypes;
    }

    public Class<? extends OWLObject> getCls() {
        switch(this){
            case classes: return OWLClass.class;
            case objectproperties: return OWLObjectProperty.class;
            case dataproperties: return OWLDataProperty.class;
            case annotationproperties: return OWLAnnotationProperty.class;
            case individuals: return OWLNamedIndividual.class;
            case datatypes: return OWLDatatype.class;
            case entities: return OWLEntity.class;
            case ontologies: return OWLOntology.class;
        }
        throw new RuntimeException("Unknown named object type: " + this);
    }

    public OWLEntity getOWLEntity(IRI iri, OWLDataFactory df){
        switch(this){
            case classes: return df.getOWLClass(iri);
            case objectproperties: df.getOWLObjectProperty(iri);
            case dataproperties: return df.getOWLDataProperty(iri);
            case annotationproperties: return df.getOWLAnnotationProperty(iri);
            case individuals: return df.getOWLNamedIndividual(iri);
            case datatypes: return df.getOWLDatatype(iri);
        }
        throw new RuntimeException("Unknown named object type: " + this);
    }


    public static NamedObjectType getType(OWLObject object){
        return getType(object.getClass());
    }

    
    public static NamedObjectType getType(Class<? extends OWLObject> cls){
        if (OWLClass.class.isAssignableFrom(cls)){
            return NamedObjectType.classes;
        }
        else if (OWLObjectProperty.class.isAssignableFrom(cls)){
            return NamedObjectType.objectproperties;
        }
        else if (OWLDataProperty.class.isAssignableFrom(cls)){
            return NamedObjectType.dataproperties;
        }
        else if (OWLAnnotationProperty.class.isAssignableFrom(cls)){
            return NamedObjectType.annotationproperties;
        }
        else if (OWLIndividual.class.isAssignableFrom(cls)){
            return NamedObjectType.individuals;
        }
        else if (OWLDatatype.class.isAssignableFrom(cls)){
            return NamedObjectType.datatypes;
        }
        else if (OWLOntology.class.isAssignableFrom(cls)){
            return NamedObjectType.ontologies;
        }
        throw new RuntimeException("Object type not known: " + cls);
    }

}
