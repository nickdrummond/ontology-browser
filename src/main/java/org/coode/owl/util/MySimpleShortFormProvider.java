package org.coode.owl.util;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 5, 2010<br><br>
 *
 * The OWL API SimpleShortFormProvider does not work if there is no fragment
 */
public class MySimpleShortFormProvider implements ShortFormProvider {

    public String getShortForm(OWLEntity owlEntity) {
        IRI iri = owlEntity.getIRI();
        String name = iri.getFragment();
        if (name == null){
            name = iri.toString();
            if (name.endsWith("/")){
                name = name.substring(0, name.length()-1);
            }
            int index = name.lastIndexOf("/");
            if (index >= 0){
                name = name.substring(index+1, name.length());
            }
        }
        return name;
    }

    public void dispose() {
        // do nothing
    }
}
