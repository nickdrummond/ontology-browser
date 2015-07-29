package org.coode.www.service;

import com.google.common.base.Optional;
import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import static java.util.Arrays.asList;

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

    public List<Characteristic> getCharacteristics(OWLDatatype owlDatatype, OWLHTMLKit kit) {
        Set<OWLOntology> activeOntologies = kit.getOWLServer().getActiveOntologies();
        Comparator<OWLObject> comparator = kit.getOWLServer().getComparator();

        CharacteristicsFactory fac = new CharacteristicsFactory();

        List<Characteristic> characteristics = new ArrayList<>();
        for (Optional<Characteristic> c : asList(
                fac.getAnnotations(owlDatatype, activeOntologies, comparator),
                fac.getDatatypeDefinitions(owlDatatype, activeOntologies, comparator),
                fac.getUsage(owlDatatype, activeOntologies, comparator)
        )) {
            if (c.isPresent()) {
                characteristics.add(c.get());
            }
        }
        return characteristics;
    }
}
