package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * sku属性
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_sku_attr")
@Schema(name = "SkuAttr", description = "sku属性")
public class SkuAttr implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    @EnableQuery
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    @EnableQuery
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    @EnableQuery
    private String createdId = "";

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    @EnableQuery
    private String updatedId = "";

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "是否对用户可见")
    @EnableQuery
    private Boolean visible;

    @Schema(description = "属性ID")
    @EnableQuery
    @TableId(value = "attr_id", type = IdType.ASSIGN_UUID)
    private String attrId;

    @Schema(description = "所属skuID")
    @EnableQuery
    private String skuId;

    @Schema(description = "属性名称")
    @EnableQuery
    private String attrName;

    @Schema(description = "属性值")
    @EnableQuery
    private String attrValue;
}
