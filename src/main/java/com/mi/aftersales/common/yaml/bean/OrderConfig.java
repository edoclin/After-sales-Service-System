package com.mi.aftersales.common.yaml.bean;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * @author edoclin
 */
@Component
@ConfigurationProperties(prefix = "order")
@Data
public class OrderConfig {
    private Long topN;
}
