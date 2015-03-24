package org.coode.html.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 7, 2009<br><br>
 */
public enum OWLHTMLProperty {

    optionContentWindow("Content window", true, "content-window"),
    optionIndexAllURL("Index all URL", true, "index-all-url"),
    optionDefaultCSS("CSS file", "css"),
    optionUseFrames("Use frames", true, "frames"),
    optionShowMiniHierarchies("Show hierarchies", "option_show_mini_hierarchies"),
    optionShowInferredHierarchies("Show inferred hierarchies", true, "option_show_inferred_hierarchies"),
    optionRenderPermalink("Render permalinks", "option_render_permalink"),
    optionRenderOntologySummaryCloud("Ontology summary cloud links", true, "option_render_ontology_summary_cloud"),
    optionRenderSubs("Render children", true, "option_render_subs"),
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
