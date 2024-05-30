package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;

/**
 * @author adgjm
 * @since 2024-05-18
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "设置Sku属性可见表单", description = "设置Sku属性可见表单")
public class SkuAttrVisibleSetForm {
    @Schema(description = "Sku属性")
    @NotEmpty(message = "Sku属性ID不能为空")
    private String attrId;

    @Schema(description = "是否可见")
    private Boolean visible = false;
}
