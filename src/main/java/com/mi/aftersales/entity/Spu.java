package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;
import com.mi.aftersales.util.query.EnableQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * <p>
 * 商品
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_spu")
@Schema(name = "Spu", description = "商品")
public class Spu implements Serializable {

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

    @Schema(description = "商品ID")
    @TableId(value = "spu_id", type = IdType.ASSIGN_UUID)
    @EnableQuery
    private String spuId;

    @Schema(description = "所属分类ID")
    @EnableQuery
    private Integer categoryId;

    @Schema(description = "展示权重")
    private Integer weight;

    @Schema(description = "商品发布日期")
    @EnableQuery
    private LocalDateTime releasedTime;

    @Schema(description = "唯一商品名称")
    @EnableQuery
    private String spuName;

    @Schema(description = "商品封面图片ID")
    private String spuCoverFileId;

    @Schema(description = "商品富文本描述")
    private String spuDesc;

    @Schema(description = "是否对客户可见")
    @EnableQuery
    private Boolean visible;
}
