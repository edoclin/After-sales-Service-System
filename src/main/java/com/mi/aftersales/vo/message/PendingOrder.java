package com.mi.aftersales.vo.message;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mi.aftersales.entity.SpuCategory;
import com.mi.aftersales.vo.result.SpuCategoryVo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/20 14:39
 **/
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "待办工单", description = "待办工单")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class PendingOrder implements Serializable {
    @Schema(description = "工单Id")
    private String orderId;
    @Schema(description = "维修物品所属分类")
    private List<SpuCategoryVo> categories = new ArrayList<>();

    @Schema(description = "工单发起时间")
    private String createdTime;
}
