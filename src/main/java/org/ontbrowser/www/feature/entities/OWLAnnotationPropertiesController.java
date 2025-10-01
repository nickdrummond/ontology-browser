package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/annotationproperties")
public class OWLAnnotationPropertiesController extends OWLPropertiesController<OWLAnnotationProperty> {

    public OWLAnnotationPropertiesController(
            OWLHTMLKit kit,
            OWLAnnotationPropertiesService service,
            CommonContent commonContent,
            CommonFragments commonFragments
    ) {
        super(kit, service, commonContent, OWLAnnotationProperty.class, commonFragments);
    }
}
