package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

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
@Schema(name = "SpuCategory", description = "商品分类")
public class SpuCategoryFormVo implements Serializable {

    @Schema(description = "是否对用户可见")
    private Boolean visible;

    @Schema(description = "展示权重")
    private Integer weight;

    @Schema(description = "父级分类编号")
    private Integer parentCategoryId;

    @NotEmpty(message = "验证码不能为空")
    @Length(min = 1, max = 256, message = "")
    @Schema(description = "分类名称")
    private String categoryName;

    @Schema(description = "分类级别")
    private Integer categoryLevel;
}
