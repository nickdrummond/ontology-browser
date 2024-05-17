package org.ontbrowser.www.model;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;

import java.util.ArrayList;
import java.util.List;

@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
@JacksonXmlRootElement(localName = "results")
public class SearchResults {

    @JacksonXmlElementWrapper(useWrapping = false)
    @JacksonXmlProperty(localName = "rs")
    private final List<SearchResult> results = new ArrayList<>();

    public SearchResults() {
    }

    public void addResult(SearchResult result) {
        results.add(result);
    }
}
