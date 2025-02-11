package org.ontbrowser.www.exception;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.model.ProjectInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.resource.NoResourceFoundException;

@RestControllerAdvice
public class ErrorHandling {

    @Autowired
    private ProjectInfo projectInfo;

    private static final Logger log = LoggerFactory.getLogger(ErrorHandling.class);

    @ExceptionHandler
    public ModelAndView handleError(
            HttpServletRequest httpRequest,
            HttpServletResponse httpResponse,
            Exception e,
            Model model) {

        if (e instanceof NoResourceFoundException) {
            httpResponse.setStatus(HttpStatus.NOT_FOUND.value());
            return null;
        }

        // TODO if request for XML or fragment, don't spit out HTML

        model.addAttribute("projectInfo", projectInfo);

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
        if (errorCode != null) {
            if (errorCode instanceof Integer code) {
                model.addAttribute("errorCode", code);
            } else {
                log.warn("Unknown error code {} : {}", errorCode.getClass(), errorCode);
                model.addAttribute("errorCode", 0);
            }
        }
        else {
            model.addAttribute("errorCode", 0);
        }

        return new ModelAndView("error");
    }
}
