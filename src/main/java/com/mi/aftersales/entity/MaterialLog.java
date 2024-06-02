package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

import com.mi.aftersales.enums.entity.MaterialActionEnum;
import io.swagger.v3.oas.annotations.media.Schema;
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
@Schema(name = "MaterialLog", description = "物料日志")
public class MaterialLog implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    private String createdId = "";

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    private String updatedId = "";

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "日志ID")
    @TableId(value = "log_id", type = IdType.ASSIGN_UUID)
    private String logId;

    @Schema(description = "物料ID")
    private String materialId;

    @Schema(description = "操作者ID")
    private String operatorId;

    @Schema(description = "日志事件;1:入库;2:出库")
    private MaterialActionEnum action;

    @Schema(description = "变动量")
    private BigDecimal delta;

    @Schema(description = "事件详情;因...入库等")
    private String logDetail;
}
