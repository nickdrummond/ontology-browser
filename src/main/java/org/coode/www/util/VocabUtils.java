package org.coode.www.util;

import org.eclipse.rdf4j.model.vocabulary.SKOSXL;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;

public class VocabUtils {

    public static boolean isSkosXLLabelAnnotation(OWLAnnotationProperty annotProp) {
        return annotProp.getIRI().toString().equals(SKOSXL.PREF_LABEL.toString());
    }
}
