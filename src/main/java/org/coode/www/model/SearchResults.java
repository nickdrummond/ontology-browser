package org.coode.www.model;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.ArrayList;
import java.util.List;

@XmlRootElement(name = "results")
public class SearchResults {

    @XmlElement(name="rs")
    private List<SearchResult> results = new ArrayList<SearchResult>();

    public SearchResults() {
    }

    public void addResult(SearchResult result) {
        results.add(result);
    }
}
