package com.mi.aftersales.pojo.vo.form;

import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.util.ArrayList;
import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "添加权限&Api表单", description = "添加权限&Api表单")
public class PermissionApiFormVo {

    @Schema(description = "权限名称")
    @EnableQuery
    private String permissionName;

    @Schema(description = "权限唯一key值")
    @EnableQuery
    private String permissionKey;
    @Schema(description = "权限可访问的Api接口Id")
    private List<Integer> apiIds = new ArrayList<>();
}
