package org.ontbrowser.www.feature.boot;

import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.Config;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.semanticweb.owlapi.model.IRI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpStatus;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.ModelAndView;

import java.io.File;
import java.net.URL;

/**
 * If the app is not configured, it will be using the defaults and showing the pizza ontology, loaded from a local resource.
 * If that is the case, the user can navigate to /boot. This page should give the users some options for configuring the app.
 * Once they submit the form, the app will persist the new configuration to an external properties file and restart with the new configuration.
 * /boot is then disabled, as the app is now configured.
 */
@RestController
@RequestMapping(value = "/boot")
@ConditionalOnProperty(name = BootController.ROOT_PROPERTY, havingValue = BootController.DEFAULT_LOC)
public class BootController {
    private static final Logger log = LoggerFactory.getLogger(BootController.class);
    public static final String DEFAULT_LOC = "examples/pizza.owl";
    public static final String ROOT_PROPERTY = "ontology.root.location";
    public static final String PROPS_FILE = "application.properties";
    public static final String PROJECT_NAME_PROP = "project.name";
    private static final String LABEL_IRI_PROPERTY = "renderer.annotation.iri";
    private static final String LANGUAGE_PROPERTY = "renderer.annotation.lang";

    private final RestartableKit kit;
    private final CommonContent commonContent;
    private final ProjectInfo projectInfo;
    private final Environment environment;

    @Autowired
    public BootController(RestartableKit kit, CommonContent commonContent, ProjectInfo projectInfo, Environment environment) {
        this.kit = kit;
        this.commonContent = commonContent;
        this.projectInfo = projectInfo;
        this.environment = environment;
    }

    @RequestMapping()
    public ModelAndView boot(Model model) {
        checkEnabled();
        var oldConfig = kit.getConfig();
        commonContent.addCommonContent("", model, kit.getRootOntology());
        model.addAttribute("title", "Boot configuration");
        model.addAttribute("currentProjectName", projectInfo.name());
        model.addAttribute("labelIRI", oldConfig.labelIRI().toString());
        model.addAttribute("language", oldConfig.labelLang());
        return new ModelAndView("boot");
    }

    private void checkEnabled() {
        String loc = environment.getProperty(ROOT_PROPERTY);
        if (!DEFAULT_LOC.equals(loc)) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND);
        }
    }

    @PostMapping()
    public ModelAndView configureAndRestart(
            @RequestParam("projectName") String projectName,
            @RequestParam("ontologyRootLocation") String newLocation,
            @RequestParam("labelIRI") String labelIRI,
            @RequestParam("language") String language,
            Model model
    ) {
        checkEnabled();
        if (!isHtmlSafe(projectName)) {
            model.addAttribute("error", "Invalid project name: " + projectName + ". Please supply a non-empty name without special characters.");
            return boot(model);
        }
        if (!isValidLocation(newLocation)) {
            model.addAttribute("error", "Invalid location: " + newLocation + ". Please supply an OWL file. Either a local file or a valid URL: ");
            return boot(model);
        }
        if (!isValidIRI(labelIRI)) {
            model.addAttribute("error", "Invalid label IRI: " + labelIRI + ". Please supply the IRI of an annotation property: ");
            return boot(model);
        }
        if (!isValidLanguage(language)) {
            model.addAttribute("error", "Invalid language: " + language + ". Please supply a valid BCP47 language code, or leave empty for no language filtering.");
            return boot(model);
        }
        persistProperties(newLocation, projectName, labelIRI, language);
        System.setProperty(ROOT_PROPERTY, newLocation);
        projectInfo.setName(projectName); // Update the bean
        log.info("Boot update. Restarting...");
        ModelAndView mv = new ModelAndView("redirect:/boot/restarting");
        // Start restart thread only after response is returned
        new Thread(() -> {
            try {
                var config = new Config(newLocation, IRI.create(labelIRI), language);
                kit.restart(config); // Actually restart the kit
            } catch (Exception e) {
                log.error("Restart thread error", e);
            }
        }).start();
        return mv;
    }

    private boolean isValidLanguage(String language) {
        return language.isEmpty() || language.matches("^[a-zA-Z]{2}$");
    }

    @RequestMapping("/restarting")
    public ModelAndView restarting() {
        return new ModelAndView("restarting");
    }

    private static void persistProperties(String newLocation, String projectName, String labelIRI, String language) {
        try (java.io.FileWriter writer = new java.io.FileWriter(PROPS_FILE, false)) {
            writer.write(PROJECT_NAME_PROP + "=" + projectName + "\n");
            writer.write(ROOT_PROPERTY + "=" + newLocation + "\n");
            writer.write(LABEL_IRI_PROPERTY + "=" + labelIRI + "\n");
            writer.write(LANGUAGE_PROPERTY + "=" + language + "\n");
            //renderer:
            //  annotation
        } catch (Exception e) {
            log.error("Failed to persist ontology.root.location", e);
        }
    }


    private boolean isHtmlSafe(String projectName) {
        if (projectName == null || projectName.isEmpty()) return false;
        if (projectName.contains("<") || projectName.contains(">") || projectName.contains("&")) return false;
        return true;
    }

    private static boolean isValidIRI(String labelIRI) {
        try {
            URL url = new URL(labelIRI);
            String protocol = url.getProtocol();
            return (protocol != null);
        } catch (Exception e) {
            return false;
        }
    }

    private static boolean isValidLocation(String location) {
        if (location == null || location.isBlank()) return false;
        if (isValidIRI(location)) {
            return true;
        }
        // Not a valid URL, check if it's a file that exists
        File file = new File(location);
        if (file.exists() && file.isFile()) {
            return true;
        }
        return false;
    }
}
