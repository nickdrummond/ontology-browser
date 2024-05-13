package org.ontbrowser.www.util;

import org.semanticweb.owlapi.model.IRI;

public class TransactionUtils {

    public static final String BASE = "http://www.ontbrowser.org/ontology/";
    public static final IRI CREATED_PROPERTY_IRI = IRI.create(BASE + "transactionCreated");
    public static final IRI TARGET_PROPERTY_IRI = IRI.create(BASE + "targetOnt");
    public static final IRI OPERATION_PROPERTY_IRI = IRI.create(BASE + "operation");

    public static IRI iriForTransaction(String transactionID) {
        return IRI.create(BASE + transactionID + "/pending" );
    }
}
