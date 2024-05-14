package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 物料
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_material")
@ApiModel(value = "Material对象", description = "物料")
public class Material implements Serializable {

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

    @ApiModelProperty("物料ID")
    @TableId(value = "material_id", type = IdType.AUTO)
    private String materialId;

    @ApiModelProperty("物料名称")
    private String materialName;

    @ApiModelProperty("物料富文本描述")
    private String materialDesc;

    @ApiModelProperty("物料封面描述图片")
    private String materialCoverFileId;

    @ApiModelProperty("计量单位")
    private String unit;

    @ApiModelProperty("物料剩余库存")
    private BigDecimal stock;

    @ApiModelProperty("物料成本")
    private BigDecimal cost;

    @ApiModelProperty("销售价格")
    private BigDecimal price;

    @ApiModelProperty("库存告警阈值")
    private BigDecimal alertNum;
}
