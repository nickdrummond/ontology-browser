package org.coode.www.model;

import com.google.common.collect.Lists;

import javax.xml.bind.annotation.*;
import java.util.Collection;

@XmlAccessorType(value = XmlAccessType.NONE)
@XmlRootElement(name = "urlset", namespace = "http://www.sitemaps.org/schemas/sitemap/0.9")
public class XmlUrlSet {

    @XmlElements({@XmlElement(name = "url", type = XmlUrl.class)})
    private final Collection<XmlUrl> xmlUrls = Lists.newArrayList();

    public void addUrl(final XmlUrl xmlUrl) {
        xmlUrls.add(xmlUrl);
    }

    public Collection<XmlUrl> getXmlUrls() {
        return xmlUrls;
    }
}