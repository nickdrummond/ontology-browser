package org.coode.www.model;

public class ReasonerMomento {

    private String label;
    private String cls;

    public ReasonerMomento(String label, String cls) {
        this.label = label;
        this.cls = cls;
    }

    public String getLabel() {
        return label;
    }

    public String getCls() {
        return cls;
    }

    @Override
    public String toString() {
        return label;
    }
}
