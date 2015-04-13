package org.coode.www.model;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

import java.util.List;

public class Characteristic {

    private OWLObject subject;

    private OWLProperty name;

    private List<OWLObject> objects;

    public Characteristic(OWLObject subject, OWLProperty predicate, List<OWLObject> objects) {
        this.subject = subject;
        this.name = predicate;
        this.objects = objects;
    }

    public OWLObject getSubject() {
        return subject;
    }

    public OWLProperty getName() {
        return name;
    }

    public List<OWLObject> getObjects() {
        return objects;
    }
}
