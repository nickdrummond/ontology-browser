package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.cloud.*;
import org.coode.html.doclet.AbstractTitleDoclet;
import org.coode.html.doclet.CloudDoclet;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.www.doclet.CloudIndexDoclet;
import org.coode.www.exception.OntServerException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
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
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 27, 2007<br><br>
 */
public class Cloud extends AbstractOntologyServerServlet {

    private String title = "Clouds";

    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; // TODO implement
    }

    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {

        String cloudParam = params.get(OWLHTMLParam.type);

        OWLDocPage page = new OWLDocPage(kit);

        page.addDoclet(new CloudIndexDoclet(kit));

        if (cloudParam != null){

            page.addDoclet(new AbstractTitleDoclet(kit){

                @Override
                public String getTitle() {
                    return title;
                }

                @Override
                public String getSubtitle() {
                    return null;
                }
            });

            CloudDoclet cloudRenderer = getCloudRenderer(params, kit, pageURL);

            if (cloudRenderer != null){
                page.addDoclet(cloudRenderer);
            }
        }
        return page;
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return getCloudRenderer(params, kit, pageURL);
    }

    private CloudDoclet getCloudRenderer(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) {
        String cloudParam = params.get(OWLHTMLParam.type);
        int threshold = OWLHTMLConstants.DEFAULT_CLOUD_THRESHOLD;
        try{
            threshold = Integer.parseInt(params.get(OWLHTMLParam.threshold));
        }
        catch (Throwable e){

        }
        int zoom = OWLHTMLConstants.DEFAULT_CLOUD_ZOOM;
        try{
            zoom = Integer.parseInt(params.get(OWLHTMLParam.zoom));
        }
        catch (Throwable e){

        }

        if (cloudParam != null){
            CloudType cType = CloudType.valueOf(cloudParam);

            OWLCloudModel cloudModel = getModelFromType(cType, kit);
            cloudModel.setOntologies(getOntologies(kit, params));

            CloudDoclet cloudRenderer = new CloudDoclet(kit);

            cloudRenderer.setModel(cloudModel);
            cloudRenderer.setComparator(kit.getOWLServer().getComparator());
            cloudRenderer.setThreshold(threshold);
            cloudRenderer.setZoom(zoom);

            title = cloudModel.getTitle();

            return cloudRenderer;
        }

        return null;
    }

    private OWLCloudModel getModelFromType(CloudType type, OWLHTMLKit kit) {
        switch(type){
            case classusage:
                return new ClassesByUsageCloud(kit);
            case objpropusage:
                return new ObjectPropsByUsageCloud(kit);
            case datapropusage:
                return new DataPropsByUsageCloud(kit);
            case annotpropusage:
                return new AnnotationPropsByUsageCloud(kit);
            case indusage:
                return new IndividualsByUsageCloud(kit);
            case datatypeusage:
                return new DatatypesByUsageCloud(kit);

        }
        return null;
    }

    public Set<OWLOntology> getOntologies(OWLHTMLKit kit, Map<OWLHTMLParam, String> params) {

        String ontURIStr = params.get(OWLHTMLParam.ontology);

        if (ontURIStr != null){
            try {
                IRI ontURI = IRI.create(new URI(ontURIStr));
                return Collections.singleton(kit.getOWLServer().getOWLOntologyManager().getOntology(ontURI));
            }
            catch (URISyntaxException e) {

            }
        }
        return kit.getVisibleOntologies();
    }
}
