package com.mi.aftersales.config.yaml.bean;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * @author edoclin
 */
@Component
@ConfigurationProperties(prefix = "init")
@Data
public class InitConfig {
    private String permissionName;
    private String permissionKey;
    private String loginMobile;
}
