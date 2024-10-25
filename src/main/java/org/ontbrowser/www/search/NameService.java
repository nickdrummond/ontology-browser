package org.ontbrowser.www.search;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLEntity;
import org.springframework.stereotype.Service;

@Service
public class NameService {

    public String getName(final OWLEntity owlEntity, final OWLHTMLKit kit) {
        return kit.getShortFormProvider().getShortForm(owlEntity);
    }
}
