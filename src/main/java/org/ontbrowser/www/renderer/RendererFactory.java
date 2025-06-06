package org.ontbrowser.www.renderer;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

public class RendererFactory {

    private final ShortFormProvider sfp;
    private final OntologyIRIShortFormProvider ontSfp;
    private final URLScheme urlScheme;
    private final OWLEntityFinder finder;

    public RendererFactory(
            ShortFormProvider sfp,
            OntologyIRIShortFormProvider ontSfp,
            URLScheme urlScheme,
            OWLEntityFinder finder
    ) {
        this.sfp = sfp;
        this.ontSfp = ontSfp;
        this.urlScheme = urlScheme;
        this.finder = finder;
    }

    public OWLHTMLRenderer getHTMLRenderer(final OWLOntology ont) {
        return new OWLHTMLRenderer(sfp, ontSfp, urlScheme, ont, finder);
    }
}
