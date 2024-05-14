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
@ApiModel(value = "Sku对象", description = "商品销售单元")
public class Sku implements Serializable {

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

    @ApiModelProperty("sku编号")
    @TableId(value = "sku_id", type = IdType.AUTO)
    private String skuId;

    @ApiModelProperty("所属spu编号")
    private String spuId;

    @ApiModelProperty("展示权重")
    private Integer weight;

    @ApiModelProperty("sku封面展示图片ID")
    private String skuCoverFileId;

    @ApiModelProperty("sku唯一展示名称")
    private String skuDisplayName;

    @ApiModelProperty("该sku是否对用户可见")
    private Boolean visible;
}
