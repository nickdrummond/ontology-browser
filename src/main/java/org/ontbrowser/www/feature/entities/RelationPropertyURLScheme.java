package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.url.RestURLScheme;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.IRIShortFormProvider;

public class RelationPropertyURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/relations";

    public RelationPropertyURLScheme(IRIShortFormProvider iriShortFormProvider) {
        super(iriShortFormProvider);
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject, OWLOntology ontology) {
        if (owlObject instanceof OWLObjectProperty objProp) {
            return ROOT_PATH
                    + "/onproperty/" + iriShortFormProvider.getShortForm(objProp.getIRI());
        }
        return super.getURLForOWLObject(owlObject, ontology);
    }
}
