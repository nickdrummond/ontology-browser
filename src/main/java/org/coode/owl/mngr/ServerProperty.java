package org.coode.owl.mngr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Deprecated
public enum ServerProperty {

    optionReasonerEnabled("Reasoner enabled", "reasoner.enabled"),
    optionReasoner("Reasoner", "option_reasoner"),
//    optionReasonerUrl("DIG reasoner URL", true, "option_reaoner_url"), // for DIG reasoners
    optionRenderer("Renderer type", "ren"),
    optionLabelUri("Label annotation", "option_label_uri"),
    optionLabelLang("Label language", "option_label_lang"),
    optionLabelPropertyUri("Label property"),
    optionActiveOnt("Active ontology", "option_active_ont"),
    optionShowOntologies("Show", true),
    optionRemote("Reasoner URL");


    private String[] altNames;

    private String shortName;

    private boolean deprecated = false;

    ServerProperty(String shortName, String ... altNames) {
        this(shortName, false, altNames);
    }

    ServerProperty(String shortName, boolean deprecated, String ... altNames) {
        this.shortName = shortName;
        this.altNames = altNames;
        this.deprecated = deprecated;
    }


    public String[] getAlternateNames() {
        return altNames;
    }

    public boolean isDeprecated(){
        return deprecated;
    }

    public static Map<String, String> generateDeprecatedNamesMap() {
        Map<String, String> map = new HashMap<String, String>();
        for (ServerProperty v : values()){
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

    public static List<ServerProperty> getCurrent() {
        List<ServerProperty> current = new ArrayList<ServerProperty>();
        for (ServerProperty p : values()){
            if (!p.isDeprecated()){
                current.add(p);
            }
        }
        return current;
    }
}
