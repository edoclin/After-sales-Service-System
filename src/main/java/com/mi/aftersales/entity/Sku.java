package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.time.LocalDateTime;
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
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    private String createdId;

    @Schema(description = "更新者")
    private String updatedId;

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "sku编号")
    @TableId(value = "sku_id", type = IdType.AUTO)
    private String skuId;

    @Schema(description = "所属spu编号")
    private String spuId;

    @Schema(description = "展示权重")
    private Integer weight;

    @Schema(description = "sku封面展示图片ID")
    private String skuCoverFileId;

    @Schema(description = "sku唯一展示名称")
    private String skuDisplayName;

    @Schema(description = "该sku是否对用户可见")
    private Boolean visible;
}
