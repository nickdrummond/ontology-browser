package org.ontbrowser.www.model.characteristics;

import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.PageData;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.List;

public class Characteristic {

    private final OWLObject subject;

    private final String name;

    private final List<AxiomWithMetadata> objects;

    private final PageData pageData;

    public Characteristic(OWLObject subject, String name, List<AxiomWithMetadata> objects) {
        this(subject, name, objects, new PageData(1, objects.size(), objects.size()));
    }

    public Characteristic(
            final OWLObject subject,
            final String name,
            final List<AxiomWithMetadata> objects,
            final PageData pageData) {
        this.subject = subject;
        this.name = name;
        this.objects = objects;
        this.pageData = pageData;
    }

    public OWLObject getSubject() {
        return subject;
    }

    public String getName() {
        return name;
    }

    public List<AxiomWithMetadata> getObjects() {
        return objects;
    }

    public PageData getPageData() {
        return pageData;
    }
}
