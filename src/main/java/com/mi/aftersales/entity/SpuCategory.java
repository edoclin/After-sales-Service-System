package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.time.LocalDateTime;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@ApiModel(value = "SpuCategory对象", description = "商品分类")
public class SpuCategory implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("创建时间")
    private LocalDateTime createdTime;

    @ApiModelProperty("更新时间")
    private LocalDateTime updatedTime;

    @ApiModelProperty("创建者")
    private String createdId;

    @ApiModelProperty("更新者")
    private String updatedId;

    @ApiModelProperty("逻辑删除")
    @TableLogic
    private Long deleted;

    @ApiModelProperty("是否对用户可见")
    private Boolean visible;

    @ApiModelProperty("分类编号")
    @TableId(value = "category_id", type = IdType.AUTO)
    private Integer categoryId;

    @ApiModelProperty("展示权重")
    private Integer weight;

    @ApiModelProperty("父级分类编号")
    private Integer parentCategoryId;

    @ApiModelProperty("分类名称")
    private String categoryName;

    @ApiModelProperty("分类级别")
    private Integer categoryLevel;
}
