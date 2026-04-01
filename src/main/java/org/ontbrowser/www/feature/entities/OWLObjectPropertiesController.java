package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import java.util.List;

@RestController
@RequestMapping(value = "/objectproperties")
public class OWLObjectPropertiesController extends OWLPropertiesController<OWLObjectProperty> {

    public OWLObjectPropertiesController(
            OWLHTMLKit kit,
            PropertiesService<OWLObjectProperty> service,
            CommonContent commonContent,
            CommonFragments commonFragments
    ) {
        super(kit, service, commonContent, OWLObjectProperty.class, commonFragments);
    }

    @GetMapping(value = "/{objectPropertyId}")
    @Override
    public ModelAndView getPage(String objectPropertyId, int pageSize, List<With> with, OWLOntology ont, Model model, HttpServletRequest request) {
        return super.getPage(objectPropertyId, pageSize, with, ont, model, request);
    }

    @GetMapping(value = "/{objectPropertyId}/fragment")
    @Override
    public ModelAndView getFragment(String objectPropertyId, int pageSize, List<With> with, OWLOntology ont, Model model, HttpServletRequest request) {
        return super.getFragment(objectPropertyId, pageSize, with, ont, model, request);
    }

    @GetMapping(value = "/{objectPropertyId}/children")
    @Override
    public ModelAndView getChildren(String objectPropertyId, OWLOntology ont, Model model) {
        return super.getChildren(objectPropertyId, ont, model);
    }
}
