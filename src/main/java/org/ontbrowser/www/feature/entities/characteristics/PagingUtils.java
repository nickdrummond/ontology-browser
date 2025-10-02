package org.ontbrowser.www.feature.entities.characteristics;

import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.PageData;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

public class PagingUtils {

    private PagingUtils() {
    }

    public static <T extends OWLObject> Characteristic getCharacteristic(
            T target,
            List<With> with,
            int defaultPageSize,
            Comparator<AxiomWithMetadata> compareByOWLObject,
            String charName,
            List<AxiomWithMetadata> results,
            OWLObject focus
    ) {
        int start = 1;
        int pageSize = defaultPageSize;
        Optional<With> withStart = getPagingFor(with, charName);
        if (withStart.isPresent()) {
            start = withStart.get().start();
            pageSize = withStart.get().pageSize();
        }
        return pagedCharacteristic(target, start, pageSize, compareByOWLObject, charName, results, focus);
    }

    private static <T extends OWLObject> Characteristic pagedCharacteristic(
            T target,
            int start,
            int pageSize,
            Comparator<AxiomWithMetadata> compareByOWLObject,
            String charName,
            List<AxiomWithMetadata> results,
            OWLObject focus
    ) {
        var sorted = results.stream()
                .sorted(compareByOWLObject)
                .toList();

        if (focus != null) {
            // ignore the start and look for the focus
            int focusIndex = -1;
            for (int i = 0; i < sorted.size(); i++) {
                if (sorted.get(i).owlObject().equals(focus)) {
                    focusIndex = i;
                    break;
                }
            }
            if (focusIndex >= 0) {
                // return the page starting on a multiple of pageSize that includes focusIndex
                int pageStartIndex = (focusIndex / pageSize) * pageSize;
                start = pageStartIndex + 1;
            }
        }

        var sortedAndPaged = sorted.stream()
                .skip(start - 1L)
                .limit(pageSize)
                .toList();

        var pageData = new PageData(start, pageSize, results.size());
        return new Characteristic(target, charName, sortedAndPaged, pageData);
    }

    private static Optional<With> getPagingFor(List<With> with, String charName) {
        String characteristicId = getIdFor(charName);
        return with.stream().filter(w -> w.characteristicName().equals(characteristicId)).findFirst();
    }

    public static String getIdFor(String componentName) {
        return componentName
                .replace(" ", "_")
                .replace("(", "_")
                .replace(")", "_")
                .toLowerCase();
    }
}
