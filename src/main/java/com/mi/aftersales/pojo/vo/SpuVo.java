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
@Schema(title = "商品信息（管理员）", description = "商品信息（管理员）")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class SpuVo {
    @Schema(description = "商品Id")
    private String spuId;
    @Schema(description = "分类Id")
    private String categoryId;
    @Schema(description = "商品名称")
    private String spuName;
    @Schema(description = "商品封面")
    private String spuCoverUrl;
    @Schema(description = "商品封面图片文件ID")
    private String spuCoverFileId;
    @Schema(description = "发布时间")
    private String releasedTime;
    @Schema(description = "创建时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "客户是否可见")
    private Boolean visible;
    @Schema(description = "商品排序权重（越大越靠前）")
    private Integer weight;
}
