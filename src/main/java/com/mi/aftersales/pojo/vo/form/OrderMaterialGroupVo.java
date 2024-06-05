package com.mi.aftersales.pojo.vo.form;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.OrderMaterialVo;
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
public class OrderMaterialGroupVo {
    @Schema(description = "OrderId")
    @View(label = "工单Id")
    private String orderId;

    @Schema(description = "创建时间")
    @View(label = "创建时间")
    private String createdTime;

    @Schema(description = "更新时间")
    @View(label = "更新时间")
    private String updatedTime;
    @Schema(description = "所需物料列表")
    @View(label = "所需物料列表")
    private PageResult<OrderMaterialVo> orderMaterialVoList = new PageResult<>();
}
