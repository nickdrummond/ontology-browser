package org.ontbrowser.www.feature.admin;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
public class LoginController {

    @GetMapping("/login")
    public ModelAndView login(HttpServletRequest request, HttpSession session) {
        String referer = request.getHeader("Referer");
        if (referer != null && !referer.contains("/login")) {
            session.setAttribute("loginRedirect", referer);
        }
        return new ModelAndView("login");
    }

    @PostMapping("/login-success")
    public String loginSuccess(HttpSession session, RedirectAttributes redirectAttributes) {
        String redirectUrl = (String) session.getAttribute("loginRedirect");
        session.removeAttribute("loginRedirect");
        if (redirectUrl == null || redirectUrl.contains("/login")) {
            redirectUrl = "/";
        }
        return "redirect:" + redirectUrl;
    }
}
