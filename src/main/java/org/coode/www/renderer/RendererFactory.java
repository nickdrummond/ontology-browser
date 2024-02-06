package org.coode.www.renderer;

import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLEntityFinder;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;

public class RendererFactory {

    private final ShortFormProvider sfp;
    private final OntologyIRIShortFormProvider ontSfp;
    private final URLScheme urlScheme;
    private final OWLEntityFinder finder;

    public RendererFactory(
            @Autowired final ShortFormProvider sfp,
            @Autowired final OntologyIRIShortFormProvider ontSfp,
            @Autowired final URLScheme urlScheme,
            @Autowired final OWLEntityFinder finder
    ) {
        this.sfp = sfp;
        this.ontSfp = ontSfp;
        this.urlScheme = urlScheme;
        this.finder = finder;
    }

    public OWLHTMLRenderer getRenderer(final OWLOntology ont) {
        return new OWLHTMLRenderer(sfp, ontSfp, urlScheme, ont, finder);
    }
}
