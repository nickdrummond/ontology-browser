package org.ontbrowser.www.feature.sitemap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlAccessorType(value = XmlAccessType.NONE)
@XmlRootElement(name = "url")
class XmlUrl {

    @XmlElement
    private String loc;

    public XmlUrl() {
    }

    public XmlUrl(String loc) {
        this.loc = loc;
    }

    public String getLoc() {
        return loc;
    }
}