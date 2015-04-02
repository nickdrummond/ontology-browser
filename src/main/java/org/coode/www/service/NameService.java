package org.coode.www.service;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLEntity;

import javax.xml.ws.ServiceMode;

@ServiceMode
public class NameService {

    public String getName(final OWLEntity owlEntity, final OWLHTMLKit kit) {
        return kit.getOWLServer().getShortFormProvider().getShortForm(owlEntity);
    }
}
