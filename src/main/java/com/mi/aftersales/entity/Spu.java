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
@ApiModel(value = "Spu对象", description = "商品")
public class Spu implements Serializable {

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

    @ApiModelProperty("商品ID")
    @TableId(value = "spu_id", type = IdType.AUTO)
    private String spuId;

    @ApiModelProperty("所属分类ID")
    private Integer categoryId;

    @ApiModelProperty("展示权重")
    private Integer weight;

    @ApiModelProperty("商品发布日期")
    private LocalDateTime releasedTime;

    @ApiModelProperty("唯一商品名称")
    private String spuName;

    @ApiModelProperty("商品封面图片ID")
    private String spuCoverFileId;

    @ApiModelProperty("商品富文本描述")
    private String spuDesc;

    @ApiModelProperty("是否对客户可见")
    private Boolean visible;
}
