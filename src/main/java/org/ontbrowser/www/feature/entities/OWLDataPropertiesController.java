package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import java.util.List;

@RestController
@RequestMapping(value = "/dataproperties")
public class OWLDataPropertiesController extends OWLPropertiesController<OWLDataProperty> {

    public OWLDataPropertiesController(
            OWLHTMLKit kit,
            PropertiesService<OWLDataProperty> service,
            CommonContent commonContent,
            CommonFragments commonFragments
    ) {
        super(kit, service, commonContent, OWLDataProperty.class, commonFragments);
    }
    
    @GetMapping(value = "/{dataPropertyId}")
    @Override
    public ModelAndView getPage(String dataPropertyId, int pageSize, List<With> with, OWLOntology ont, Model model, HttpServletRequest request) {
        return super.getPage(dataPropertyId, pageSize, with, ont, model, request);
    }

    @GetMapping(value = "/{dataPropertyId}/fragment")
    @Override
    public ModelAndView getFragment(String dataPropertyId, int pageSize, List<With> with, OWLOntology ont, Model model, HttpServletRequest request) {
        return super.getFragment(dataPropertyId, pageSize, with, ont, model, request);
    }

    @GetMapping(value = "/{dataPropertyId}/children")
    @Override
    public ModelAndView getChildren(String dataPropertyId, OWLOntology ont, Model model) {
        return super.getChildren(dataPropertyId, ont, model);
    }
}