package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = "/objectproperties")
public class OWLObjectPropertiesController extends OWLPropertiesController<OWLObjectProperty> {

    public OWLObjectPropertiesController(
            OWLHTMLKit kit,
            PropertiesService<OWLObjectProperty> service,
            CommonContent commonContent
    ) {
        super(kit,
                service,
                commonContent,
                OWLObjectProperty.class,
                kit.getOWLOntologyManager().getOWLDataFactory().getOWLTopObjectProperty());
    }
}
