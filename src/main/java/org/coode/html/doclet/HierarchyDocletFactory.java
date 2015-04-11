package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.html.doclet.*;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.*;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 4, 2010<br><br>
 */
@Deprecated
public class HierarchyDocletFactory {

    private OWLHTMLKit kit;

    public HierarchyDocletFactory(OWLHTMLKit kit) {
        this.kit = kit;
    }

    @SuppressWarnings("unchecked")
    public <N extends OWLObject> HierarchyDoclet<N> getHierarchy(Class<N> cls){
        HierarchyProvider<N> hp = kit.getOWLServer().getHierarchyProvider(cls);
        String title = NamedObjectType.getType(cls).getPluralRendering();

        if (kit.getHTMLProperties().isSet(OWLHTMLProperty.optionShowInferredHierarchies)){
            title += " (Inferred)";
        }

        HierarchyDoclet<N> hierarchyDoclet = new HierarchyDoclet<N>(title, kit, hp);
        hierarchyDoclet.setAutoExpandEnabled(true);
        return hierarchyDoclet;
    }
}
