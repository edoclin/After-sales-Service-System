package com.mi.aftersales.util.query;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "分页查询", description = "分页查询")
public class ConditionQuery {
    @Schema(description = "分页查询当前页")
    private Long current = 1L;
    @Schema(description = "分页查询页面大小")
    private Long limit = 10L;
    @Schema(description = "高级检索条件")
    private List<QueryParam> params = null;
}
