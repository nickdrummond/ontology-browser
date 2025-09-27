package org.ontbrowser.www.controller;

import org.ontbrowser.www.kit.impl.RestartableKit;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/status")
public class StatusController {

    private final RestartableKit kit;

    public StatusController(RestartableKit kit) {
        this.kit = kit;
    }

    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    @ResponseBody
    public AppStatus status() {
        return kit.getStatus();
    }
}
