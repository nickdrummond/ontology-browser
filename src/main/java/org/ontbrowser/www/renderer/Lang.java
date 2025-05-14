package org.ontbrowser.www.renderer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Lang {

    private Lang() {
        // Prevent instantiation
    }

    public static <P> Map<P, List<String>> createLangMap(P p, String lang) {
        final Map<P, List<String>> lMap = new HashMap<>();
        if (!lang.isEmpty()){
            List<String> langs = new ArrayList<>();
            langs.add(lang);
            langs.add(""); // default to no language
            lMap.put(p, langs);
        }
        return lMap;
    }
}
