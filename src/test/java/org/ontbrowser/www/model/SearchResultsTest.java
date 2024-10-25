package org.ontbrowser.www.model;

import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import org.junit.Test;
import org.ontbrowser.www.feature.search.SearchResult;
import org.ontbrowser.www.feature.search.SearchResults;

import java.io.IOException;

import static junit.framework.TestCase.assertEquals;

public class SearchResultsTest {

    @Test
    public void serialize_SearchResult() throws IOException {
        SearchResult sr = new SearchResult("id", "info", "label");

        XmlMapper mapper = new XmlMapper();
        String result = mapper.writeValueAsString(sr);

        assertEquals("<rs id=\"id\" info=\"info\">label</rs>", result);
    }

    @Test
    public void serialize_SearchResults() throws IOException {
        SearchResults sr = new SearchResults();
        sr.addResult(new SearchResult("id", "info", "label"));
        sr.addResult(new SearchResult("id2", "info2", "label2"));

        XmlMapper mapper = new XmlMapper();
        String result = mapper.writeValueAsString(sr);

        assertEquals("<results><rs id=\"id\" info=\"info\">label</rs><rs id=\"id2\" info=\"info2\">label2</rs></results>", result);
    }
}