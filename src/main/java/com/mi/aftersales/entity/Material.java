package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import io.swagger.v3.oas.annotations.media.Schema;
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
@Schema(name = "Material", description = "物料")
public class Material implements Serializable {

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

    @Schema(description = "物料ID")
    @TableId(value = "material_id", type = IdType.ASSIGN_UUID)
    private String materialId;

    @Schema(description = "物料名称")
    private String materialName;

    @Schema(description = "物料富文本描述")
    private String materialDesc;

    @Schema(description = "物料封面描述图片")
    private String materialCoverFileId;

    @Schema(description = "计量单位")
    private String unit;

    @Schema(description = "物料剩余库存")
    private BigDecimal stock;

    @Schema(description = "物料成本")
    private BigDecimal cost;

    @Schema(description = "销售价格")
    private BigDecimal price;

    @Schema(description = "库存告警阈值")
    private BigDecimal alertNum;
}
