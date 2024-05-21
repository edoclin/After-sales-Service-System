package com.mi.aftersales.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "工单所需物料", description = "工单所需物料")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class MaterialNum {
    @Schema(description = "物料Id")
    private String materialId;

    @Schema(description = "物料数量")
    private BigDecimal num;

    @Schema(description = "库管备注说明（分发）")
    private String desc;
}
