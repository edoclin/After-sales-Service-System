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
@Schema(title = "商品Sku信息（管理员）", description = "商品Sku信息（管理员）")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class SkuVo {
    @Schema(description = "商品SPU_ID")
    private String spuId;
    @Schema(description = "商品SKU_ID")
    private String skuId;
    @Schema(description = "商品名称")
    private String skuDisplayName;
    @Schema(description = "商品封面")
    private String skuCoverUrl;
    @Schema(description = "商品封面图片文件ID")
    private String skuCoverFileId;
    @Schema(description = "创建时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "客户是否可见")
    private Boolean visible;
    @Schema(description = "商品排序权重（越大越靠前）")
    private Integer weight;
}
