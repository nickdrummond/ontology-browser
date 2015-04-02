package org.coode.www.service;

import org.coode.owl.mngr.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

@Service
public class OWLDatatypesService {

    // TODO need to index the entities by ID
    public OWLDatatype getOWLDatatypeFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        OWLDatatype owlTopDatatype = df.getTopDatatype();
        if (getIdFor(owlTopDatatype).equals(propertyId)) {
            return owlTopDatatype;
        }
        
        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLDatatype owlDatatype: ont.getDatatypesInSignature()) {
                if (getIdFor(owlDatatype).equals(propertyId)){
                    return owlDatatype;
                }
            }
        }
        throw new NotFoundException("OWLDatatype", propertyId);
    }

    public String getIdFor(final OWLDatatype owlDatatype) {
        return String.valueOf(owlDatatype.getIRI().hashCode());
    }

    public HierarchyProvider<OWLDatatype> getHierarchyProvider(final OWLHTMLKit kit) {
        return kit.getOWLServer().getHierarchyProvider(OWLDatatype.class);
    }
}
