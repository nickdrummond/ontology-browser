package org.ontbrowser.www.feature.sitemap;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import java.util.function.Consumer;

@RestController
@RequestMapping(value="/sitemap.xml")
public class SitemapController extends ApplicationController {

    @GetMapping()
    public XmlUrlSet sitemap() {
        final String baseUrl = ServletUriComponentsBuilder.fromCurrentContextPath().build().toUriString();

        XmlUrlSet xmlUrlSet = new XmlUrlSet();

        // base
        xmlUrlSet.addUrl(new XmlUrl(baseUrl));

        // indices
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/ontologies"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/classes"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/individuals"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/objectproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/dataproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/annotationproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/datatypes"));

        // additional pages
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/dlquery/"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/classes"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/individuals"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/objectproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/dataproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/annotationproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/datatypes"));

        OWLOntology ont = kit.getRootOntology();
        URLScheme scheme = kit.getURLScheme();

        // ontologies
        ont.getImportsClosure().forEach(o -> xmlUrlSet.addUrl(new XmlUrl(baseUrl + scheme.getURLForOWLObject(o))));

        // entities
        Consumer<OWLEntity> action = i -> xmlUrlSet.addUrl(new XmlUrl(baseUrl + scheme.getURLForOWLObject(i)));
        ont.getClassesInSignature(Imports.INCLUDED).forEach(action);
        ont.getIndividualsInSignature(Imports.INCLUDED).forEach(action);
        ont.getObjectPropertiesInSignature(Imports.INCLUDED).forEach(action);
        ont.getDataPropertiesInSignature(Imports.INCLUDED).forEach(action);
        ont.getAnnotationPropertiesInSignature(Imports.INCLUDED).forEach(action);
        ont.getDatatypesInSignature(Imports.INCLUDED).forEach(action);

        return xmlUrlSet;
    }
}
