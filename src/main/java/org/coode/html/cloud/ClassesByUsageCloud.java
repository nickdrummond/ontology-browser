package org.coode.html.cloud;

import org.coode.html.OWLHTMLKit;
import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;


/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 15, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class ClassesByUsageCloud extends AbstractOWLCloudModel<OWLClass>{

    private URLScheme urlScheme;

    public ClassesByUsageCloud(OWLHTMLKit kit) {
        super(kit.getOWLServer().getShortFormProvider());
        this.urlScheme = kit.getURLScheme();
        setOntologies(kit.getVisibleOntologies());
    }

    public Set<OWLClass> getEntities() {
        Set<OWLClass> owlClasses = new HashSet<OWLClass>();
        for (OWLOntology ont : getOntologies()) {
            owlClasses.addAll(ont.getClassesInSignature());
        }
        return owlClasses;
    }

    public URL getURL(OWLClass entity) {
        return urlScheme.getURLForOWLObject(entity);
    }

    public String getTitle() {
        return CloudType.classusage.getRendering();
    }

    protected int calculateValue(OWLClass entity) {
        int count = 0;
        for (OWLOntology ont : getOntologies()){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}