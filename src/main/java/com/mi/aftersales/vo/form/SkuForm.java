package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "管理员添加商品SKU", description = "管理员添加商品SKU")
public class SkuForm {
    @Schema(description = "商品SKU_ID")
    @NotNull(message = "商品SKU_ID不能为空")
    private String spuId;

    @Schema(description = "商品SKU展示权重（越大排序越靠前）")
    private Short weight = 0;

    @Schema(description = "商品SKU名称")
    @NotEmpty(message = "商品SKU名称不能为空")
    private String skuDisplayName;

    @Schema(description = "商品Spu封面图片ID")
    @NotEmpty(message = "商品Spu封面图片ID不能为空")
    private String skuCoverFileId;

    @Schema(description = "客户是否可见")
    private Boolean visible = false;
}
