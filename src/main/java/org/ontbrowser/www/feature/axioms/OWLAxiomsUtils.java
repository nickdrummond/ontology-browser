package org.ontbrowser.www.feature.axioms;

import org.semanticweb.owlapi.model.AxiomType;

import java.util.Comparator;
import java.util.stream.Stream;

import static org.ontbrowser.www.util.MyStringUtils.splitCamelCase;

class OWLAxiomsUtils {

    // Prevent instantiation
    private OWLAxiomsUtils() {
    }

    public static final String LOGICAL_AXIOMS_TYPE = "logicalAxioms";

    record AxiomTypeData(AxiomType<?> type, String name, String category) {
    }

    public static Stream<AxiomTypeData> getAxiomTypes() {
        return AxiomType.AXIOM_TYPES.stream()
                .map(t -> new AxiomTypeData(
                        t,
                        splitCamelCase(t.getName()),
                        getCategory(t)
                ))
                .sorted(Comparator.comparing(AxiomTypeData::category).thenComparing(AxiomTypeData::name));
    }

    private static String getCategory(AxiomType<?> t) {
        if (AxiomType.TBoxAxiomTypes.contains(t))
            return "T";
        if (AxiomType.ABoxAxiomTypes.contains(t))
            return "A";
        if (AxiomType.RBoxAxiomTypes.contains(t))
            return "R";
        return "-";
    }
}
