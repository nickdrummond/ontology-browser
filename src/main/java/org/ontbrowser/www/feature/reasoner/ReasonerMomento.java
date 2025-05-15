package org.ontbrowser.www.feature.reasoner;

public class ReasonerMomento {

    private final String label;
    private final String cls;

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
