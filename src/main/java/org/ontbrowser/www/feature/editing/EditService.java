package org.ontbrowser.www.feature.editing;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

@Service
public class EditService {

    public OWLOntology add(OWLAxiom newAxiom, OWLOntology targetOnt, String transactionID, OWLOntology rootOntology) {

        targetOnt.addAxiom(newAxiom);

        return targetOnt;
    }

    public OWLOntology edit(
            OWLAxiom originalAxiom, OWLAxiom newAxiom,
            OWLOntology originalOnt, OWLOntology targetOnt,
            String transactionID, OWLOntology rootOntology) {

        originalOnt.removeAxiom(originalAxiom);
        targetOnt.addAxiom(newAxiom);

        return targetOnt;
    }

    public OWLOntology remove(
            OWLAxiom originalAxiom,
            OWLOntology originalOnt,
            String transactionID,
            OWLOntology rootOntology) {
        originalOnt.removeAxiom(originalAxiom);
        return originalOnt;
    }
}
