package org.coode.www.util;

import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.PageData;
import org.coode.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

public class PagingUtils {

    public static <T extends OWLObject> Characteristic getCharacteristic(
            T target,
            List<With> with,
            int defaultPageSize,
            Comparator<AxiomWithMetadata> compareByOWLObject,
            String charName,
            List<AxiomWithMetadata> results) {

        int start = 1;
        int pageSize = defaultPageSize;
        Optional<With> withStart = getPagingFor(with, charName);
        if (withStart.isPresent()) {
            start = withStart.get().start();
            pageSize = withStart.get().pageSize();
        }
        return pagedCharacteristic(target, start, pageSize, compareByOWLObject, charName, results);
    }

    private static <T extends OWLObject> Characteristic pagedCharacteristic(
            T target,
            int start,
            int pageSize,
            Comparator<AxiomWithMetadata> compareByOWLObject,
            String charName,
            List<AxiomWithMetadata> results) {

        List<AxiomWithMetadata> sortedAndPaged = results.stream()
                .sorted(compareByOWLObject)
                .skip(start-1L)
                .limit(pageSize)
                .toList();

        PageData pageData = new PageData(start, pageSize, results.size());

        return new Characteristic(target, charName, sortedAndPaged, pageData);
    }

    private static Optional<With> getPagingFor(List<With> with, String charName) {
        String characteristicId = getIdFor(charName);
        return with.stream().filter(w -> w.characteristicName().equals(characteristicId)).findFirst();
    }

    public static String getIdFor(String componentName) {
        return componentName.replace(" ", "_").toLowerCase();
    }

}
