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
 * 商品分类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_spu_category")
@Schema(name = "SpuCategory", description = "商品分类")
public class SpuCategory implements Serializable {

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

    @Schema(description = "分类编号")
    @TableId(value = "category_id", type = IdType.AUTO)
    @EnableQuery
    private Integer categoryId;

    @Schema(description = "展示权重")
    @EnableQuery
    private Integer weight;

    @Schema(description = "父级分类编号")
    @EnableQuery
    private Integer parentCategoryId;

    @Schema(description = "分类名称")
    @EnableQuery
    private String categoryName;

    @Schema(description = "分类级别")
    @EnableQuery
    private Integer categoryLevel;
}
