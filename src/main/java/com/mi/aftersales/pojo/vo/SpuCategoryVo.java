package com.mi.aftersales.pojo.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "商品分类", description = "商品分类")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class SpuCategoryVo implements Serializable {
    @Schema(description = "分类ID")
    private Integer categoryId;
    @Schema(description = "分类名称")
    private String categoryName;
}
