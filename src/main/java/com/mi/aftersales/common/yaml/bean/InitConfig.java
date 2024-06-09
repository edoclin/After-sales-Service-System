package com.mi.aftersales.common.yaml.bean;

import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.List;

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
    private List<EmployeeRoleEnum> roleList;
}
