package org.coode.www.service;

import org.semanticweb.owlapi.model.IRI;
import org.springframework.stereotype.Service;

@Service
public class MediaService {

    public boolean isImageURL(IRI iri) {
        String iriStr = iri.toString();
        return iriStr.endsWith(".png") ||
                iriStr.endsWith(".gif") ||
                iriStr.endsWith(".jpg") ||
                iriStr.endsWith(".jpeg");
    }

    public boolean isSoundURL(IRI iri) {
        String iriStr = iri.toString();
        return iriStr.endsWith(".mp3") ||
                iriStr.endsWith(".wav");
    }
}
