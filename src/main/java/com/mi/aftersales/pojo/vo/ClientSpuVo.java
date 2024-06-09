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
@Schema(title = "商品信息（客户）", description = "商品信息（客户）")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ClientSpuVo {
    @Schema(description = "商品Id")
    private String spuId;
    @Schema(description = "分类Id")
    private String categoryId;
    @Schema(description = "商品名称")
    private String spuName;
    @Schema(description = "商品封面")
    private String spuCoverUrl;
    @Schema(description = "发布时间")
    private String releasedTime;
}
