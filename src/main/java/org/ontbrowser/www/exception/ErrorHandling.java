package org.ontbrowser.www.exception;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.model.ProjectInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.resource.NoResourceFoundException;

@RestControllerAdvice
public class ErrorHandling {

    private static final Logger log = LoggerFactory.getLogger(ErrorHandling.class);
    public static final String MODEL_ERROR_CODE = "errorCode";
    public static final String MODEL_MESSAGE = "message";

    private final ProjectInfo projectInfo;

    public ErrorHandling(ProjectInfo projectInfo) {
        this.projectInfo = projectInfo;
    }

    @ExceptionHandler(produces = "text/html")
    public ModelAndView handleError(
            HttpServletRequest httpRequest,
            HttpServletResponse httpResponse,
            Exception e,
            Model model) {

        model.addAttribute("projectInfo", projectInfo);

        if (log.isDebugEnabled()) {
            httpRequest.getAttributeNames().asIterator().forEachRemaining(n -> {
                log.debug("request attr = {}", n);
            });
        }
        if (e != null) {
            if (e instanceof NoResourceFoundException) {
                httpResponse.setStatus(HttpStatus.NOT_FOUND.value());
                return null;
            }

            log.error(e.getMessage());

            if (e instanceof ResponseStatusException statusException) {
                log.error("Status exception {}", statusException.getMessage());
                int status = statusException.getStatusCode().value();
                httpResponse.setStatus(status);
                model.addAttribute(MODEL_ERROR_CODE, status);
                model.addAttribute(MODEL_MESSAGE, statusException.getMessage());
                return new ModelAndView("error");
            }
        }

        Object exception = httpRequest.getAttribute("jakarta.servlet.error.exception");
        if (exception instanceof Exception ex) {
            log.error("Servlet/container error {}", ex.getMessage(), ex);
        }

        Object errorCode = httpRequest.getAttribute("jakarta.servlet.error.status_code");
        if (errorCode != null) {
            if (errorCode instanceof Integer code) {
                model.addAttribute(MODEL_ERROR_CODE, code);
            } else {
                log.warn("Unknown error code {} : {}", errorCode.getClass(), errorCode);
                model.addAttribute(MODEL_ERROR_CODE, 0);
            }
        } else {
            model.addAttribute(MODEL_ERROR_CODE, 0);
        }
        model.addAttribute(MODEL_MESSAGE, "Sorry, an error has occurred.");

        return new ModelAndView("error");
    }
}
