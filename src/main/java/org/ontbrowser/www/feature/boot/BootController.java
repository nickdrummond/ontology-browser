package org.ontbrowser.www.feature.boot;

import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.Config;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
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

    private final OWLHTMLKit kit;
    private final CommonContent commonContent;
    private final ProjectInfo projectInfo;
    private final Environment environment;

    @Autowired
    public BootController(OWLHTMLKit kit, CommonContent commonContent, ProjectInfo projectInfo, Environment environment) {
        this.kit = kit;
        this.commonContent = commonContent;
        this.projectInfo = projectInfo;
        this.environment = environment;
    }

    @RequestMapping()
    public ModelAndView boot(Model model) {
        checkEnabled();
        commonContent.addCommonContent("", model, kit.getRootOntology());
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
            Model model
    ) {
        checkEnabled();
        if (!isHtmlSafe(projectName)) {
            log.warn("Rejected invalid projectName: {}", projectName);
            model.addAttribute("error", "Invalid project name: " + projectName + ". Please supply a non-empty name without special characters.");
            return boot(model);
        }
        if (!isValidLocation(newLocation)) {
            log.warn("Rejected invalid ontologyRootLocation: {}", newLocation);
            model.addAttribute("error", "Invalid location: " + newLocation + ". Please supply an OWL file. Either a local file or a valid URL: ");
            return boot(model);
        }
        persistProperties(newLocation, projectName);
        System.setProperty(ROOT_PROPERTY, newLocation);
        projectInfo.setName(projectName); // Update the bean
        log.info("Boot update. Restarting...");
        Thread restartThread = new Thread(() -> {
            try {
                var oldConfig = kit.getConfig();
                var config = new Config(newLocation, oldConfig.labelIRI(), oldConfig.labelLang());
                kit.restart(config); // Actually restart the kit
            } catch (Exception e) {
                log.error("Restart thread error", e);
            }
        });
        restartThread.setDaemon(false);
        restartThread.start();
        return new ModelAndView("redirect:/boot/restarting");
    }

    @RequestMapping("/restarting")
    public ModelAndView restarting() {
        return new ModelAndView("restarting");
    }

    private static void persistProperties(String newLocation, String projectName) {
        try (java.io.FileWriter writer = new java.io.FileWriter(PROPS_FILE, false)) {
            writer.write(PROJECT_NAME_PROP + "=" + projectName + "\n");
            writer.write(ROOT_PROPERTY + "=" + newLocation + "\n");
        } catch (Exception e) {
            log.error("Failed to persist ontology.root.location", e);
        }
    }


    private boolean isHtmlSafe(String projectName) {
        if (projectName == null || projectName.isEmpty()) return false;
        if (projectName.contains("<") || projectName.contains(">") || projectName.contains("&")) return false;
        return true;
    }

    private static boolean isValidLocation(String location) {
        if (location == null || location.isBlank()) return false;
        try {
            // Check if it's a valid URL
            URL url = new URL(location);
            String protocol = url.getProtocol();
            if (protocol.equals("http") || protocol.equals("https") || protocol.equals("file")) {
                return true;
            }
        } catch (Exception e) {
            // Not a valid URL, check if it's a file that exists
            File file = new File(location);
            if (file.exists() && file.isFile()) {
                return true;
            }
        }
        return false;
    }
}
