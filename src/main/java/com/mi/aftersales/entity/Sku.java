package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.time.LocalDateTime;

import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 商品销售单元
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_sku")
@Schema(name = "Sku", description = "商品销售单元")
public class Sku implements Serializable {

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

    @Schema(description = "sku编号")
    @TableId(value = "sku_id", type = IdType.ASSIGN_UUID)
    @EnableQuery
    private String skuId;

    @Schema(description = "所属spu编号")
    @EnableQuery
    private String spuId;

    @Schema(description = "展示权重")
    @EnableQuery
    private Integer weight;

    @Schema(description = "sku封面展示图片ID")
    private String skuCoverFileId;

    @Schema(description = "sku唯一展示名称")
    @EnableQuery
    private String skuDisplayName;

    @Schema(description = "该sku是否对用户可见")
    @EnableQuery
    private Boolean visible;
}
