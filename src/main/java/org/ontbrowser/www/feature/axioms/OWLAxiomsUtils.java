package org.ontbrowser.www.feature.axioms;

import org.ontbrowser.www.url.PagingURIScheme;
import org.semanticweb.owlapi.model.AxiomType;

import java.net.URI;
import java.util.Comparator;
import java.util.stream.Stream;

import static org.ontbrowser.www.util.MyStringUtils.splitCamelCase;

class OWLAxiomsUtils {

    // Prevent instantiation
    private OWLAxiomsUtils() {
    }

    public static final String LOGICAL_AXIOMS_TYPE = "logicalAxioms";

    record AxiomTypeData(AxiomType<?> type, String name, String category, URI link) {
    }

    public static Stream<AxiomTypeData> getAxiomTypes(PagingURIScheme scheme) {
        return AxiomType.AXIOM_TYPES.stream()
                .map(t -> new AxiomTypeData(
                        t,
                        splitCamelCase(t.getName()),
                        getCategory(t),
                        scheme.builder()
                                .replaceQueryParam("type", t.toString())
                                .replaceQueryParam("start", 1)
                                .build(true).toUri()
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
