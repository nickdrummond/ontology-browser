package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.CharacteristicsBuilder;
import org.ontbrowser.www.feature.entities.characteristics.DatatypeCharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;

@Service
public class OWLDatatypesService implements CharacteristicsProvider<OWLDatatype> {

    public CharacteristicsBuilder<OWLDatatype> getCharacteristicsBuilder(
            final OWLDatatype owlDatatype,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new DatatypeCharacteristicsBuilder(owlDatatype, ont, comparator, with, pageSize);
    }

    @Override
    public List<Characteristic> getInferredCharacteristics(OWLDatatype entity, OWLReasoner reasoner) {
            return List.of();
    }
}
