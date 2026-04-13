package org.ontbrowser.www.configuration;

import org.semanticweb.owlapi.model.IRI;

public record Config(
        String root,
        IRI labelIRI,
        String labelLang
) {
}
