package com.mi.aftersales.config.yaml.bean;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * @author edoclin
 */
@Component
@ConfigurationProperties(prefix = "custom.sms")
@Data
public class CustomSmsConfig {
    /**
     * @description: 验证码有效时间(Second)
     * @return:
     * @author: edoclin
     * @created: 2024/5/14 23:11
     **/
    private Long validTime;
    /**
     * @description: 发送间隔周期
     * @return:
     * @author: edoclin
     * @created: 2024/5/14 23:11
     **/
    private Long period;

    private Boolean enable = Boolean.FALSE;
}
