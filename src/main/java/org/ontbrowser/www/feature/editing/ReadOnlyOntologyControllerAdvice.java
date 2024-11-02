package org.ontbrowser.www.feature.editing;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.ontologies.OWLOntologiesService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@ControllerAdvice
@Profile("!editing")
public class ReadOnlyOntologyControllerAdvice {

    private final OWLHTMLKit kit;

    private final OWLOntologiesService ontService;

    public ReadOnlyOntologyControllerAdvice(
            @Autowired OWLHTMLKit kit,
            @Autowired OWLOntologiesService ontService
    ) {
        this.kit = kit;
        this.ontService = ontService;
    }

    @ModelAttribute
    public OWLOntology injectParent(@RequestParam(required = false) final String ontId) throws NotFoundException {
        if (ontId != null) {
            return ontService.getOntologyFor(ontId, kit);
        }
        return kit.getRootOntology();
    }

}