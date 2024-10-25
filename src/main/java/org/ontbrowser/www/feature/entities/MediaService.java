package org.ontbrowser.www.feature.entities;

import org.semanticweb.owlapi.model.IRI;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

@Service
public class MediaService {

    private final List<String> imgs = Arrays.asList(".png",  ".jpg", ".jpeg", ".gif");
    private final List<String> sounds = Arrays.asList(".mp3",  ".wav");

    public boolean isImageURL(IRI iri) {
        String iriStr = iri.toString().toLowerCase();
        return imgs.stream().anyMatch(iriStr::endsWith);
    }

    public boolean isSoundURL(IRI iri) {
        String iriStr = iri.toString().toLowerCase();
        return sounds.stream().anyMatch(iriStr::endsWith);
    }
}
