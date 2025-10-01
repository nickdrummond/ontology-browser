package org.ontbrowser.www.feature.entities;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.feature.entities.characteristics.CharacteristicsBuilder;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.ui.ConcurrentModel;
import org.springframework.ui.Model;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class CommonFragmentsTest {

    @Mock
    private OWLHTMLKit mockKit;
    @Mock
    private ReasonerService mockReasonerService;
    @Mock
    private OWLIndividualsService mockIndividualsService;
    @Mock
    private OWLOntology mockOnt;
    @Mock
    private ShortFormProvider mockShortFormProvider;
    @Mock
    private CharacteristicsBuilder<OWLNamedIndividual> mockCharacteristicsBuilder;

    private CommonFragments commonFragments;

    private ProjectInfo stubProjectInfo;
    private OWLDataFactory df;

    @BeforeEach
    public void setUp() {
        stubProjectInfo = new ProjectInfo("name", "contact", "url", "tagline", List.of());
        commonFragments = new CommonFragments(mockKit, stubProjectInfo, mockReasonerService);
        df = OWLManager.getOWLDataFactory();
        when(mockKit.getShortFormProvider()).thenReturn(mockShortFormProvider);
        when(mockIndividualsService.getCharacteristicsBuilder(any(), any(), any(), anyList(), anyInt())).thenReturn(mockCharacteristicsBuilder);
        when(mockCharacteristicsBuilder.getCharacteristics()).thenReturn(List.of());
    }
    @Test
    public void testTitleIsLabelPlusNamedType() {
        // GIVEN the individual has a named type
        var ind = df.getOWLNamedIndividual("http://null.org/ind");
        var clsType = df.getOWLClass("http://null.org/cls");

        when(mockIndividualsService.getNamedTypes(ind, mockOnt)).thenReturn(Set.of(clsType));

        when(mockShortFormProvider.getShortForm(ind)).thenReturn("label");
        when(mockShortFormProvider.getShortForm(clsType)).thenReturn("type");

        // WHEN we get the fragment
        Model model = new ConcurrentModel();
        commonFragments.getOWLIndividualFragment(mockIndividualsService, ind,
                false, List.of(), mockOnt, model, "");

        // THEN the title is the label of the individual and its type
        assertEquals("label (type)", model.getAttribute("title"));
    }
}