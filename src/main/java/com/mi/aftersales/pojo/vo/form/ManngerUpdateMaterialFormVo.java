package com.mi.aftersales.pojo.vo.form;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import java.math.BigDecimal;

/**
 * @author QYenon
 * @create 2024/5/21
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "库管更新物料信息",description = "库管更新物料信息")
public class ManngerUpdateMaterialFormVo {


    @Schema(description = "物料ID")
    @NotEmpty(message = "物料ID不能为空")
    private String materialId;

    @Schema(description = "物料名称")
    @Length(min = 1, max = 256, message = "")
    private String materialName;

    @Schema(description = "物料富文本描述")
    private String materialDesc;

    @Schema(description = "物料封面描述图片")
    private String materialCoverFileId;


    @Schema(description = "物料所属Spu分类Id")
    private Integer spuCategoryId;

    @Schema(description = "计量单位")
    private String unit;

    @Schema(description = "添加库存数量")
    @Min(value = 0, message = "添加库存数量必须大于等于0")
    private BigDecimal stock;

    @Schema(description = "物料成本")
    @Min(value = 0, message = "物料成本必须大于等于0")
    private BigDecimal cost;

    @Schema(description = "销售价格")
    @Min(value = 0, message = "销售价格必须大于等于0")
    private BigDecimal price;

    @Schema(description = "库存告警阈值")
    @Min(value = 0, message = "库存告警阈值必须大于等于0")
    private BigDecimal alertNum;
}
