package com.mi.aftersales.pojo.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 商品分类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(name = "商品分类树状查询", description = "商品分类树状查询")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class SpuCategory4ClientVo implements Serializable {

    @Schema(description = "分类编号")
    private Integer categoryId;

    @Schema(description = "展示权重")
    private Integer weight;

    @Schema(description = "分类名称")
    private String categoryName;

    @Schema(description = "树状目录")
    private List<SpuCategory4ClientVo> children;
}
