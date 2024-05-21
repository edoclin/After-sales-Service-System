package com.mi.aftersales.vo.form;

import com.mi.aftersales.vo.MaterialNum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "库管分发物料", description = "库管分发物料")
public class MaterialDistributeForm {
    @Schema(description = "工单Id")
    @NotEmpty(message = "工单Id")
    private String orderId;

    @Schema(description = "分发物料数量")
    private List<MaterialNum> materials;
}
