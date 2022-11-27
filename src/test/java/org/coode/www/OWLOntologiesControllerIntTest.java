package org.coode.www;

import org.coode.www.controller.OWLOntologiesController;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.OWLOntologiesService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.ContextHierarchy;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.servlet.config.annotation.DefaultServletHandlerConfigurer;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
import static org.mockito.Mockito.mock;

@RunWith(SpringJUnit4ClassRunner.class)
@WebAppConfiguration
@ContextHierarchy({
        @ContextConfiguration(classes = OWLOntologiesControllerIntTest.OntologyControllerConfig.class),
        @ContextConfiguration(classes = OWLOntologiesControllerIntTest.WebConfig.class)
})
public class OWLOntologiesControllerIntTest {

    @Configuration
    @EnableWebMvc
    static class WebConfig implements WebMvcConfigurer {
        @Override
        public void configureDefaultServletHandling(DefaultServletHandlerConfigurer configurer) {
            configurer.enable();
        }
    }

    @Configuration
    @EnableWebMvc
    static class OntologyControllerConfig extends AppControllerConfig {

        @Bean
        public OWLOntologiesService ontologiesService() {
            return mock(OWLOntologiesService.class);
        }

        @Bean
        public OntologyIRIShortFormProvider ontologyIRIShortFormProvider() {
            return mock(OntologyIRIShortFormProvider.class);
        }

        @Bean
        public OWLHTMLKit owlhtmlKit() {
            return mock(OWLHTMLKit.class);
        }

        @Bean
        public OWLOntologiesController ontologiesController() {
            return new OWLOntologiesController();
        }
    }

    @Autowired
    private WebApplicationContext wac;

    @Autowired
    private OWLOntologiesService mockOWLOntologiesService;

    private MockMvc mockMvc;

    @Before
    public void setup() {
        this.mockMvc = MockMvcBuilders.webAppContextSetup(this.wac).build();
    }

    @Test
    public void getOntology() throws Exception {
        OWLOntology testOntology = mock(OWLOntology.class);
        when(mockOWLOntologiesService.getOntologyFor(eq("blah"), any(OWLHTMLKit.class))).thenReturn(testOntology);

        this.mockMvc.perform(get("/ontologies/blah"))
                .andExpect(status().isOk());
    }
}
