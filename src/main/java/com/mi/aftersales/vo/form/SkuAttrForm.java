package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;

/**
 * @author adgjm
 * @since 2024-05-18
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "管理员添加商品SKU属性", description = "管理员添加商品SKU属性")
public class SkuAttrForm {
    @Schema(description = "商品SKU_ID")
    @NotNull(message = "商品SKU_ID不能为空")
    private String skuId;

    @Schema(description = "SKU属性名称")
    private String attrName;

    @Schema(description = "SKU属性名称值")
    private String attrValue;

    @Schema(description = "客户是否可见")
    private Boolean visible = false;
}
