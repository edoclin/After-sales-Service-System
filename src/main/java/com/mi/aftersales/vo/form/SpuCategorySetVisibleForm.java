package com.mi.aftersales.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "设置分类可见表单", description = "设置分类可见表单")
public class SpuCategorySetVisibleForm {

    @Schema(description = "分类ID")
    private Integer categoryId = 0;

    @Schema(description = "是否可见")
    private Boolean visible = false;

}
