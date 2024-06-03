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
@Schema(title = "商品SKU属性（管理员）", description = "商品SKU属性（管理员）")
public class SkuAttrVo {
    @Schema(description = "商品SKU属性ID")
    private String attrId;
    @Schema(description = "商品SKU_ID")
    private String skuId;
    @Schema(description = "属性名称")
    private String attrName;
    @Schema(description = "属性值")
    private String attrValue;
    @Schema(description = "创建时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "客户是否可见")
    private Boolean visible;
}
