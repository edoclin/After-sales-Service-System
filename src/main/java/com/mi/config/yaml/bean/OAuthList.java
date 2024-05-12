package com.mi.config.yaml.bean;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * @author edoclin
 */
@Component
@ConfigurationProperties(prefix = "oauth")
@Data
public class OAuthList {
    private Map<String, OAuthConfig> clients;
}
