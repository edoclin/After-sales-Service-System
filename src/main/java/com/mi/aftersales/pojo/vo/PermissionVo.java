package com.mi.aftersales.pojo.vo;

import com.baomidou.mybatisplus.annotation.*;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "权限", description = "权限")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class PermissionVo {
    @Schema(description = "创建时间")
    @EnableQuery
    private String createdTime;

    @Schema(description = "更新时间")
    @EnableQuery
    private String updatedTime;

    @Schema(description = "创建者")
    @EnableQuery
    private String createdId = "";

    @Schema(description = "更新者")
    @EnableQuery
    private String updatedId = "";

    @Schema(description = "权限ID")
    @EnableQuery
    private Integer permissionId;

    @Schema(description = "权限名称")
    @EnableQuery
    private String permissionName;

    @Schema(description = "权限唯一key值")
    @EnableQuery
    private String permissionKey;
}
