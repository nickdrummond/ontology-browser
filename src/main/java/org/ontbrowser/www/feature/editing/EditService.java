package org.ontbrowser.www.feature.editing;

import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Date;

@Service
public class EditService {

    /**
     * TODO take a transaction ID from the frontend to keep adding to the same ontology
     */
    public OWLOntology add(OWLAxiom newAxiom, OWLOntology targetOnt, String transaction, OWLOntology rootOntology) {
//        OWLOntologyManager mngr = targetOnt.getOWLOntologyManager();
//        OWLOntology ont = getOntologyForTransaction(mngr, transaction, rootOntology);
//        OWLDataFactory df = mngr.getOWLDataFactory();

//        OWLAxiom annotated = newAxiom.getAnnotatedAxiom(Set.of(
//                df.getOWLAnnotation(
//                        df.getOWLAnnotationProperty(TransactionUtils.TARGET_PROPERTY_IRI),
//                        df.getOWLLiteral(targetOnt.getOntologyID().getOntologyIRI().get().toString())
//                ),
//                df.getOWLAnnotation(
//                        df.getOWLAnnotationProperty(TransactionUtils.OPERATION_PROPERTY_IRI),
//                        df.getOWLLiteral("ADD")
//                )
//        ));

        targetOnt.addAxiom(newAxiom);

        return targetOnt;
    }

    /**
     * This is just an add at the minute- one axiom in a new ontology
     * So, we need the following:
     * - need to remove the original axiom - with an annotated REMOVE
     */
    public OWLOntology edit(
            OWLAxiom originalAxiom, OWLAxiom newAxiom,
            OWLOntology originalOnt, OWLOntology targetOnt,
            String transactionID, OWLOntology rootOntology) {
//
//        OWLOntologyManager mngr = originalOnt.getOWLOntologyManager();
//        OWLOntology ont = getOntologyForTransaction(mngr, transactionID, rootOntology);
//        OWLDataFactory df = mngr.getOWLDataFactory();
//
//        OWLAxiom annotated = newAxiom.getAnnotatedAxiom(Set.of(
//                df.getOWLAnnotation(
//                        df.getOWLAnnotationProperty(TransactionUtils.TARGET_PROPERTY_IRI),
//                        df.getOWLLiteral(targetOnt.getOntologyID().getOntologyIRI().get().toString())
//                ),
//                df.getOWLAnnotation(
//                        df.getOWLAnnotationProperty(TransactionUtils.OPERATION_PROPERTY_IRI),
//                        df.getOWLLiteral("ADD")
//                )
//        ));

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

    private OWLOntology getOntologyForTransaction(
            OWLOntologyManager mngr,
            String transactionID,
            OWLOntology rootOntology) {
        IRI transactionOntologyIRI = TransactionUtils.iriForTransaction(transactionID);
        OWLOntology ont = mngr.getOntology(transactionOntologyIRI);
        if (ont == null) {
            try {
                ont = mngr.createOntology(transactionOntologyIRI);
                OWLDataFactory df = mngr.getOWLDataFactory();
                OWLAnnotation created = df.getOWLAnnotation(
                        df.getOWLAnnotationProperty(TransactionUtils.CREATED_PROPERTY_IRI),
                        df.getOWLLiteral(Date.from(Instant.now()).toString())
                );
                mngr.applyChange(new AddOntologyAnnotation(ont, created));
                mngr.applyChange(new AddImport(ont, df.getOWLImportsDeclaration(rootOntology.getOntologyID().getOntologyIRI().get())));
            } catch (OWLOntologyCreationException e) {
                throw new RuntimeException(e); // Cannot create transaction ontology
            }
        }
        return ont;
    }
}
