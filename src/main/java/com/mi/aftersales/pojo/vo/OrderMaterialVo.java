package com.mi.aftersales.pojo.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mi.aftersales.util.view.anno.View;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "工单物料表", description = "工单物料表")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class OrderMaterialVo {
    @Schema(description = "Id")
    @View(label = "id")
    private String id;

    @Schema(description = "OrderId")
    @View(label = "所属工单Id")
    private String orderId;

    @Schema(description = "创建时间")
    @View(label = "创建时间")
    private String createdTime;

    @Schema(description = "更新时间")
    @View(label = "更新时间")
    private String updatedTime;

    @Schema(description = "物料Id")
    @View(label = "物料Id")
    private String materialId;

    @Schema(description = "物料名称")
    @View(label = "物料名称")
    private String materialName;

    @Schema(description = "所需数量")
    @View(label = "所需数量")
    private String materialAmount;

    @Schema(description = "申请人")
    @View(label = "申请人")
    private String createdId;

    @Schema(description = "申请人联系方式")
    @View(label = "申请人联系方式")
    private String mobile;
}
