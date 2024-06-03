package com.mi.aftersales.pojo.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @author adgjm
 * @since 2024-05-18
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "商品Sku属性（用户）", description = "商品Sku属性（用户）")
public class ClientSkuAttrVo {
    @Schema(description = "商品Sku属性Id")
    private String attrId;
    @Schema(description = "商品SkuId")
    private String skuId;
    @Schema(description = "属性名称")
    private String attrName;
    @Schema(description = "属性值")
    private String attrValue;
}
