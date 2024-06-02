package com.mi.aftersales.pojo.vo;

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
@Schema(title = "待处理工单列表（工程师）", description = "待处理工单列表（工程师）")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class PendingOrderSimple4EngineerVo {
    @Schema(description = "工单Id")
    private String orderId;

    @Schema(description = "商品Id")
    private String spuId;

    @Schema(description = "商品Spu名称")
    private String spuName;

    @Schema(description = "商品Sku名称")
    private String skuDisplayName;

    @Schema(description = "分类名称")
    private List<String> categories = new ArrayList<>();

    @Schema(description = "创建时间")
    private String createdTime;

    @Schema(description = "工单类型")
    private String orderType;

    @Schema(description = "客户故障描述")
    private String clientFaultDesc;


}
