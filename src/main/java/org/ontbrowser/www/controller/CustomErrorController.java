package org.ontbrowser.www.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletRequest;

@RestController
public class CustomErrorController extends ApplicationController {

    private static final Logger log = LoggerFactory.getLogger(CustomErrorController.class);

    @GetMapping("/error")
    public ModelAndView handleError(
            HttpServletRequest httpRequest,
            Exception e,
            Model model) {

        // TODO if request for XML or fragment, don't spit out HTML

        if (log.isDebugEnabled()) {
            httpRequest.getAttributeNames().asIterator().forEachRemaining(n -> {
                log.debug("request attr = " + n);
            });
        }

        if (e != null) {
            log.error("Application error", e);
        }

        Object exception = httpRequest.getAttribute("jakarta.servlet.error.exception");
        if (exception instanceof Exception ex) {
            log.error("Servlet/container error " + ex.getMessage(), ex);
        }

        Object errorCode = httpRequest.getAttribute("jakarta.servlet.error.status_code");
        if (errorCode instanceof Integer code) {
            model.addAttribute("errorCode", code);
        }
        else {
            log.warn("Unknown error code {} : {}", errorCode.getClass(), errorCode);
            model.addAttribute("errorCode", 0);
        }

        return new ModelAndView("error");
    }
}