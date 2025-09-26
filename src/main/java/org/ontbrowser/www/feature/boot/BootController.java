package org.ontbrowser.www.feature.boot;

import org.ontbrowser.www.kit.Config;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
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
@ConditionalOnProperty(prefix = "ontology.root", name = "location", havingValue = "examples/pizza.owl")
public class BootController {
    private static final Logger log = LoggerFactory.getLogger(BootController.class);

    private final OWLHTMLKit kit;

    @Autowired
    public BootController(OWLHTMLKit kit) {
        this.kit = kit;
    }

    @RequestMapping()
    public ModelAndView boot() {
        return new ModelAndView("boot");
    }

    @PostMapping()
    public ModelAndView configureAndRestart(
            @RequestParam("ontologyRootLocation") String newLocation,
            Model model
    ) {
        if (!isValidOntologyLocation(newLocation)) {
            log.warn("Rejected invalid ontologyRootLocation: {}", newLocation);
            model.addAttribute("error", "Invalid location: " + newLocation + ". Please supply an OWL file. Either a local file or a valid URL: ");
            return new ModelAndView("boot");
        }
        persistProperties(newLocation);
        System.setProperty("ontology.root.location", newLocation);
        log.info("Set ontology.root.location to {}. Restarting context...", newLocation);
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

    private static void persistProperties(String newLocation) {
        try (java.io.FileWriter writer = new java.io.FileWriter("application.properties", false)) {
            writer.write("ontology.root.location=" + newLocation + "\n");
        } catch (Exception e) {
            log.error("Failed to persist ontology.root.location", e);
        }
    }

    private static boolean isValidOntologyLocation(String location) {
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

    @RequestMapping("/restarting")
    public ModelAndView restarting() {
        return new ModelAndView("restarting");
    }
}
