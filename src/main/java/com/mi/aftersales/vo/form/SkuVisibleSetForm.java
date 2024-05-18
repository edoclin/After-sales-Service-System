package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "设置商品Sku可见表单", description = "设置商品Sku可见表单")
public class SkuVisibleSetForm {

    @Schema(description = "商品SkuID")
    @NotEmpty(message = "商品SkuID不能为空")
    private String skuId;

    @Schema(description = "是否可见")
    private Boolean visible = false;

}
