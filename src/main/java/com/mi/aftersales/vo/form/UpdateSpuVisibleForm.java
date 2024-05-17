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
@Schema(title = "更新商品SPU是否可见", description = "更新商品SPU是否可见")
public class UpdateSpuVisibleForm {
    @Schema(description = "商品SPU编号")
    @NotEmpty(message = "商品SPU不能为空")
    private String spuId;

    @Schema(description = "客户是否可见")
    private Boolean visible;

}
