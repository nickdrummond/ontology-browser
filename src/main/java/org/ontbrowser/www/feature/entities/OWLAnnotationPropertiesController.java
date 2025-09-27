package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/annotationproperties")
public class OWLAnnotationPropertiesController extends OWLPropertiesController<OWLAnnotationProperty> {

    public OWLAnnotationPropertiesController(
            RestartableKit kit,
            OWLAnnotationPropertiesService service,
            CommonContent commonContent
    ) {
        super(kit, service, commonContent, OWLAnnotationProperty.class);
    }
}
