package org.coode.www.model;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlValue;

public class SearchResult {

    @XmlAttribute
    private String id;

    @XmlAttribute
    private String info;

    @XmlValue
    private String label;

    public SearchResult() {
    }

    public SearchResult(String id, String info, String label) {
        this.id = id;
        this.info = info;
        this.label = label;
    }
}
