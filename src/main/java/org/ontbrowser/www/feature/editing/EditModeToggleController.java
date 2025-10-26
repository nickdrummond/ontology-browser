package org.ontbrowser.www.feature.editing;

import jakarta.servlet.http.HttpServletRequest;
import org.springframework.context.annotation.Profile;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

/**
 * Controller to allow admins to toggle edit mode for their session.
 */
@Profile("editing")
@Controller
@PreAuthorize("hasRole('ADMIN')")
@RequestMapping("/admin/edit-mode")
public class EditModeToggleController {
    @GetMapping
    public String toggleEditMode(@RequestParam(name = "enable", required = false) Boolean enable,
                                 HttpServletRequest request,
                                 RedirectAttributes redirectAttributes) {
        if (Boolean.TRUE.equals(enable)) {
            request.getSession(true).setAttribute("editMode", true);
            redirectAttributes.addFlashAttribute("message", "Edit mode enabled.");
        } else {
            request.getSession(true).removeAttribute("editMode");
            redirectAttributes.addFlashAttribute("message", "Edit mode disabled.");
        }
        String targetUrl = request.getHeader("Referer");
        if (targetUrl != null && !targetUrl.isBlank()) {
            return "redirect:" + targetUrl;
        }
        return "redirect:/";
    }
}
