package com.mi.aftersales.config.yaml.bean;

import lombok.Data;

/**
 * @author edoclin
 */
@Data
public class OAuthConfig {
    private String clientId;
    private String clientSecret;
    private String callbackUri;
}
