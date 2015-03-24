package org.coode.owl.mngr;

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
