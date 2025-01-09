package org.ontbrowser.www.feature.search;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlText;


@JsonAutoDetect(fieldVisibility = JsonAutoDetect.Visibility.ANY)
@JacksonXmlRootElement(localName = "rs")
public class SearchResult {

    @JacksonXmlProperty(isAttribute = true)
    private String id;

    @JacksonXmlProperty(isAttribute = true)
    private String info;

    @JacksonXmlText
    private String label;

    public SearchResult() {
    }

    public SearchResult(String id, String info, String label) {
        this.id = id;
        this.info = info;
        this.label = label;
    }


}
