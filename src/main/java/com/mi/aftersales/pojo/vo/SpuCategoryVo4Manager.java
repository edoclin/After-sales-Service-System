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
public class SpuCategoryVo4Manager implements Serializable {
    @Schema(description = "分类ID")
    private Integer categoryId;
    @Schema(description = "分类名称")
    private String categoryName;

    @Schema(description = "客户是否可见")
    private Boolean visible;

    @Schema(description = "创建时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;

    @Schema(description = "创建者")
    private String createdId;
    @Schema(description = "更新者")
    private String updatedId;

    @Schema(description = "父级分类Id")
    private Integer parentCategoryId;

    @Schema(description = "分类层级")
    private Integer categoryLevel;
}
