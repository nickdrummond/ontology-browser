package org.ontbrowser.www.feature.editing;

import org.ontbrowser.www.feature.editing.parser.MOSAxiomTreeParser;
import org.ontbrowser.www.feature.expression.AutocompleteService;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.manchestersyntax.renderer.ParserException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;
import uk.co.nickdrummond.parsejs.AutocompleteResult;
import uk.co.nickdrummond.parsejs.ParseException;

import java.time.Instant;
import java.util.Date;
import java.util.Set;

@Service
public class EditService {

    private final AutocompleteService autocompleteService;

    public EditService(AutocompleteService autocompleteService) {
        this.autocompleteService = autocompleteService;
    }

    /**
     * TODO take a transaction ID from the frontend to keep adding to the same ontology
     */
    public OWLOntology add(OWLAxiom newAxiom, OWLOntology targetOnt, String transaction, OWLOntology rootOntology) {
        OWLOntologyManager mngr = targetOnt.getOWLOntologyManager();
        OWLOntology ont = getOntologyForTransaction(mngr, transaction, rootOntology);
        OWLDataFactory df = mngr.getOWLDataFactory();

        OWLAxiom annotated = newAxiom.getAnnotatedAxiom(Set.of(
                df.getOWLAnnotation(
                        df.getOWLAnnotationProperty(TransactionUtils.TARGET_PROPERTY_IRI),
                        df.getOWLLiteral(targetOnt.getOntologyID().getOntologyIRI().get().toString())
                ),
                df.getOWLAnnotation(
                        df.getOWLAnnotationProperty(TransactionUtils.OPERATION_PROPERTY_IRI),
                        df.getOWLLiteral("ADD")
                )
        ));

        ont.addAxiom(annotated);

        return ont;
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

        OWLOntologyManager mngr = originalOnt.getOWLOntologyManager();
        OWLOntology ont = getOntologyForTransaction(mngr, transactionID, rootOntology);
        OWLDataFactory df = mngr.getOWLDataFactory();

        OWLAxiom annotated = newAxiom.getAnnotatedAxiom(Set.of(
                df.getOWLAnnotation(
                        df.getOWLAnnotationProperty(TransactionUtils.TARGET_PROPERTY_IRI),
                        df.getOWLLiteral(targetOnt.getOntologyID().getOntologyIRI().get().toString())
                ),
                df.getOWLAnnotation(
                        df.getOWLAnnotationProperty(TransactionUtils.OPERATION_PROPERTY_IRI),
                        df.getOWLLiteral("ADD")
                )
        ));

        ont.addAxiom(annotated);

        return ont;
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


    public OWLAxiom parseAxiom(String axiom, OWLDataFactory df, OWLEntityChecker checker) throws ParseException {
        try {
            return new MOSAxiomTreeParser(df, checker).parse(axiom);
        } catch (ParserException e) {
            throw new ParseException(axiom, e.getMessage(), e.getStartPos(), e.getCurrentToken());
        }
    }

    public AutocompleteResult autocompleteAxiom(final String axiom,
                                                final OWLDataFactory df,
                                                final OWLEntityChecker owlEntityChecker,
                                                final OWLEntityFinder finder,
                                                final ShortFormProvider sfp) {

        try {
            new MOSAxiomTreeParser(df, owlEntityChecker).parse(axiom);
            throw new RuntimeException("Cannot get here if we have correctly forced an error " + axiom);
        } catch (ParserException e) {
            return autocompleteService.exceptionToAutocomplete(axiom, e, finder, sfp);
        }
    }
}
