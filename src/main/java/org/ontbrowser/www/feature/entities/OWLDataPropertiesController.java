package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/dataproperties")
public class OWLDataPropertiesController extends OWLPropertiesController<OWLDataProperty> {

    public OWLDataPropertiesController(
            OWLHTMLKit kit,
            PropertiesService<OWLDataProperty> service,
            CommonContent commonContent,
            ProjectInfo projectInfo
    ) {
        super(kit, service, commonContent, OWLDataProperty.class, projectInfo);
    }
}