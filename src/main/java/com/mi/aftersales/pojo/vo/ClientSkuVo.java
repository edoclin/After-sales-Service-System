package com.mi.aftersales.pojo.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "商品SKU信息（客户）", description = "商品SKU信息（客户）")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ClientSkuVo {
    @Schema(description = "商品SPU_ID")
    private String spuId;
    @Schema(description = "商品SkU_ID")
    private String skuId;
    @Schema(description = "商品名称")
    private String skuDisplayName;
    @Schema(description = "商品封面")
    private String skuCoverUrl;
}
