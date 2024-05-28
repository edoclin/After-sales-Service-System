package com.mi.aftersales.vo.result;

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
@Schema(title = "客户查询商品分类", description = "客户查询商品分类")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class ClientSpuCategoryVo {
    private String catId;
    private String catName;
    private Integer catType = 1;
    private Boolean showPic = true;
    private Boolean showVideo = false;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "发票详情")
    private String fapiaoInfo;
}
