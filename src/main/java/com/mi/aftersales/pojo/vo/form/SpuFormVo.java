package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "管理员添加商品", description = "管理员添加商品")
public class SpuFormVo {
    @Schema(description = "商品所属分类ID")
    @NotNull(message = "商品所属分类ID不能为空")
    private Integer categoryId;

    @Schema(description = "商品展示权重（越大排序越靠前）")
    private Short weight = 0;

    @Schema(description = "商品发布时间")
    private LocalDateTime releasedTime = LocalDateTime.now();

    @Schema(description = "商品名称")
    @NotEmpty(message = "商品名称不能为空")
    private String spuName;

    @Schema(description = "商品封面图片ID")
    @NotEmpty(message = "商品封面图片ID不能为空")
    private String spuCoverFileId;

    @Schema(description = "商品信息描述")
    private String spuDesc = "";

    @Schema(description = "客户是否可见")
    private Boolean visible = false;
}
