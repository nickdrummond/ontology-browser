package org.ontbrowser.www.feature.axioms;

import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;

import java.util.Comparator;
import java.util.stream.Stream;

import static org.ontbrowser.www.util.MyStringUtils.splitCamelCase;

public class OWLAxiomsUtils {

    public static final String LOGICAL_AXIOMS_TYPE = "logicalAxioms";

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

    private AxiomType<OWLAxiom> getAxiomType(String type) {
        if (type != null) {
            var axiomType = (AxiomType<OWLAxiom>) AxiomType.getAxiomType(type);
            if (axiomType == null) {
                throw new IllegalArgumentException("Unknown axiom type: " + type);
            }
            return axiomType;
        }
        return null;
    }
}
