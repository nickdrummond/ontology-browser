package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import java.util.List;

@RestController
@RequestMapping(value = "/annotationproperties")
public class OWLAnnotationPropertiesController extends OWLPropertiesController<OWLAnnotationProperty> {

    public OWLAnnotationPropertiesController(
            BackendContext backend,
            OWLAnnotationPropertiesService service,
            CommonContent commonContent,
            CommonFragments commonFragments
    ) {
        super(backend, service, commonContent, OWLAnnotationProperty.class, commonFragments);
    }
    
    @GetMapping(value = "/{annotationPropertyId}")
    @Override
    public ModelAndView getPage(String annotationPropertyId, int pageSize, List<With> with, OWLOntology ont, Model model, HttpServletRequest request) {
        return super.getPage(annotationPropertyId, pageSize, with, ont, model, request);
    }

    @GetMapping(value = "/{annotationPropertyId}/fragment")
    @Override
    public ModelAndView getFragment(String annotationPropertyId, int pageSize, List<With> with, OWLOntology ont, Model model, HttpServletRequest request) {
        return super.getFragment(annotationPropertyId, pageSize, with, ont, model, request);
    }

    @GetMapping(value = "/{annotationPropertyId}/children")
    @Override
    public ModelAndView getChildren(String annotationPropertyId, OWLOntology ont, Model model) {
        return super.getChildren(annotationPropertyId, ont, model);
    }
}
