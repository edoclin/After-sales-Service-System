package com.mi.aftersales.vo.result;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "物料查询", description = "物料查询")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class MaterialVo {
    @Schema(description = "物料Id")
    private String materialId;
    @Schema(description = "物料名称")
    private String materialName;
    @Schema(description = "创建时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "所属Spu")
    private String arrivalTime;
    @Schema(description = "所属SpuId")
    private Integer spuCategoryId;
    @Schema(description = "物料描述")
    private String materialDesc;
    @Schema(description = "图片url")
    private String coverUrl;
    private String materialCoverFileId;
    @Schema(description = "库存")
    private String stock;
    @Schema(description = "计量单位")
    private String unit;
    @Schema(description = "成本")
    private String cost;
    @Schema(description = "库存报警数量")
    private String alertNum;
    @Schema(description = "变动日志")
    private List<MaterialLogVo> logs = new ArrayList<>();

}
