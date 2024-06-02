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
@Schema(title = "更新商品Sku", description = "更新商品Sku")
public class UpdateSkuFormVo {
    @Schema(description = "Sku编号")
    @NotEmpty(message = "Sku编号不能为空")
    private String skuId;

    @Schema(description = "Spu编号")
    @NotEmpty(message = "Spu编号不能为空")
    private String spuId;

    @Schema(description = "展示权重")
    private Integer weight = 0;

    @Schema(description = "Sku展示图片Id")
    @NotEmpty(message = "Sku展示图片Id不能为空")
    private String skuCoverFileId;


    @Schema(description = "Sku名称")
    @NotEmpty(message = "Sku名称不能为空")
    private String skuDisplayName;

}
