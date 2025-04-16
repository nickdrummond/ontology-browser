package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLEntity;

import java.util.List;

public interface SearchService {

    List<OWLEntity> findByName(String input, int size, OWLHTMLKit kit);
}
