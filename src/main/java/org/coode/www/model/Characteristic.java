package org.coode.www.model;

import org.semanticweb.owlapi.model.OWLObject;

import java.util.List;

public class Characteristic {

    private OWLObject subject;

    private String name;

    private List<? extends OWLObject> objects;

    public Characteristic() {
    }

    public Characteristic(OWLObject subject, String name, List<? extends OWLObject> objects) {
        this.subject = subject;
        this.name = name;
        this.objects = objects;
    }

    public OWLObject getSubject() {
        return subject;
    }

    public void setSubject(OWLObject subject) {
        this.subject = subject;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<? extends OWLObject> getObjects() {
        return objects;
    }

    public void setObjects(List<? extends OWLObject> objects) {
        this.objects = objects;
    }
}
