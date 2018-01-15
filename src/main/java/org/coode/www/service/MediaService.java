package org.coode.www.service;

import org.semanticweb.owlapi.model.IRI;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class MediaService {

    private List<String> imgs = Arrays.asList(".png",  ".jpg", ".jpeg", ".gif");
    private List<String> sounds = Arrays.asList(".mp3",  ".wav");

    public boolean isImageURL(IRI iri) {
        String iriStr = iri.toString().toLowerCase();
        return imgs.stream().anyMatch(iriStr::endsWith);
    }

    public boolean isSoundURL(IRI iri) {
        String iriStr = iri.toString().toLowerCase();
        return sounds.stream().anyMatch(iriStr::endsWith);
    }
}
