package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.CharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Comparator;
import java.util.List;

public interface CharacteristicsProvider<T extends OWLEntity> {
    CharacteristicsBuilder<T> getCharacteristicsBuilder(
            final T entity,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int defaultPageSize
    );

    List<Characteristic> getInferredCharacteristics(
            T entity,
            OWLReasoner reasoner
    );
 }
