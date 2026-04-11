package org.ontbrowser.www.interceptor;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.feature.ontologies.OWLOntologiesService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.HandlerMapping;

import javax.annotation.Nonnull;
import java.util.Map;

@Component
public class LegacyEntityIdInterceptor implements HandlerInterceptor {

    private final OWLOntologiesService ontService;
    private final OWLHTMLKit kit;

    // Map of path variable name → entity class it resolves to
    private static final Map<String, Class<? extends OWLEntity>> PARAM_ENTITY_TYPES = Map.of(
            "classId", OWLClass.class,
            "individualId", OWLNamedIndividual.class,
            "objectPropertyId", OWLObjectProperty.class,
            "dataPropertyId", OWLDataProperty.class,
            "annotationPropertyId", OWLAnnotationProperty.class,
            "datatypeId", OWLDatatype.class
    );

    public LegacyEntityIdInterceptor(OWLOntologiesService ontService, OWLHTMLKit kit) {
        this.ontService = ontService;
        this.kit = kit;
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean preHandle(@Nonnull HttpServletRequest request, @Nonnull HttpServletResponse response, @Nonnull Object handler) {
        var pathVars = (Map<String, String>) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        if (pathVars == null) return true;

        OWLOntology ont = null;
        String newUri = request.getRequestURI();
        boolean redirectNeeded = false;

        for (var entry : PARAM_ENTITY_TYPES.entrySet()) {
            String id = pathVars.get(entry.getKey());
            if (id != null && !id.contains(":")) {
                if (ont == null) ont = getOnt(request); // lazy — only resolved if a legacy ID is found
                var entity = kit.lookup().entityFor(id, ont, entry.getValue());
                String newId = kit.getIriShortFormProvider().getShortForm(entity.getIRI());
                newUri = newUri.replace(id, newId);
                redirectNeeded = true;
            }
        }

        if (redirectNeeded) {
            String qs = request.getQueryString();
            response.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
            response.setHeader("Location", newUri + (qs != null ? "?" + qs : ""));
            return false;
        }

        return true;
    }

    private OWLOntology getOnt(HttpServletRequest request) {
        var ontId = request.getParameter("ontId");
        if (ontId != null) {
            return ontService.getOntologyFor(ontId, kit);
        }
        return kit.getRootOntology();
    }
}
