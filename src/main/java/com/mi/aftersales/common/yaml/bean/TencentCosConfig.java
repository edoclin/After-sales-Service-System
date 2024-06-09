package com.mi.aftersales.common.yaml.bean;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * @author edoclin
 */
@Component
@ConfigurationProperties(prefix = "tencent.cos")
@Data
public class TencentCosConfig {
    private String bucketName;
    private String region;
    private String prefix;
    private String secretId;
    private String secretKey;
    private String tempPath;
}
