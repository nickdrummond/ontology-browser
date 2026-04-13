package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.backend.OWLEntityFinder;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.List;

public interface SearchService {

    List<OWLEntity> findByName(
            String input,
            int size,
            OWLEntityFinder finder,
            ShortFormProvider sfp
    );
}
