package com.mi.aftersales.common.yaml.bean;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * @author edoclin
 */
@Component
@ConfigurationProperties(prefix = "alipay")
@Data
public class AliPayConfig {
    private String appId;
    private String buyerId;
    private String privateKey;
    private String publicKey;
    private String alipayGatewayUrl;
    private String notifyUrl;
    private String returnUrl;
    private String signType;
}
