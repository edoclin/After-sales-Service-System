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
@Schema(title = "工程师接受的工单列表", description = "工程师接受的工单列表")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class OrderSimple4EngineerVo {
    @Schema(description = "工单Id")
    private String orderId;

    @Schema(description = "商品Id")
    private String spuId;

    @Schema(description = "商品Spu名称")
    private String spuName;

    @Schema(description = "商品Sku名称")
    private String skuDisplayName;

    @Schema(description = "产品所属分类")
    private List<SpuCategoryVo> categories = new ArrayList<>();

    @Schema(description = "创建时间")
    private String createdTime;

    @Schema(description = "更新时间")
    private String updatedTime;

    @Schema(description = "工单类型")
    private String orderType;

    @Schema(description = "工单状态")
    private String orderStatus;
    private Integer orderStatusValue;

    @Schema(description = "客户故障描述")
    private String clientFaultDesc;


}
