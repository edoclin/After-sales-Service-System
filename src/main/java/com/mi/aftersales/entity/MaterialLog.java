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
 * 物料日志
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_material_log")
@ApiModel(value = "MaterialLog对象", description = "物料日志")
public class MaterialLog implements Serializable {

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

    @ApiModelProperty("日志ID")
    @TableId(value = "log_id", type = IdType.AUTO)
    private String logId;

    @ApiModelProperty("物料ID")
    private String materialId;

    @ApiModelProperty("操作者ID")
    private String operatorId;

    @ApiModelProperty("日志事件;1:入库;2:出库")
    private Byte action;

    @ApiModelProperty("变动量")
    private BigDecimal delta;

    @ApiModelProperty("事件详情;因...入库等")
    private String logDetail;
}
