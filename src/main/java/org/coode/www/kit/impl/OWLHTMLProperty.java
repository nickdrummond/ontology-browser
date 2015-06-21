package org.coode.www.kit.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Deprecated
public enum OWLHTMLProperty {

    optionShowMiniHierarchies("Show hierarchies", "option_show_mini_hierarchies"),
    optionShowInferredHierarchies("Show inferred hierarchies", true, "option_show_inferred_hierarchies"),
    optionRenderPermalink("Render permalinks", "option_render_permalink"),
    optionShowInferences("Show inferences");

    private String[] altNames;

    private String shortName;

    private boolean deprecated;

    OWLHTMLProperty(String shortName, String ... altNames) {
        this(shortName, false, altNames);
    }

    OWLHTMLProperty(String shortName, boolean deprecated, String ... altNames) {
        this.shortName = shortName;
        this.altNames = altNames;
        this.deprecated = deprecated;
    }

    public String[] getAlternateNames() {
        return altNames;
    }

    public static Map<String, String> generateDeprecatedNamesMap() {
        Map<String, String> map = new HashMap<String, String>();
        for (OWLHTMLProperty v : values()){
            for (String altName : v.getAlternateNames()){
                map.put(altName, v.name());
            }
        }
        return map;
    }

    @Override
    public String toString() {
        return shortName;
    }

    public static List<OWLHTMLProperty> getCurrent() {
        List<OWLHTMLProperty> current = new ArrayList<OWLHTMLProperty>();
        for (OWLHTMLProperty p : values()){
            if (!p.isDeprecated()){
                current.add(p);
            }
        }
        return current;
    }

    public boolean isDeprecated() {
        return deprecated;
    }
}
