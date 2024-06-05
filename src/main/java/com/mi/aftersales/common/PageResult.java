package com.mi.aftersales.common;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

/**
 * @description: 分页查询返回数据
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "分页返回数据", description = "分页返回数据")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class PageResult<T> {
    @Schema(description = "总数据数量")
    private Long total;
    @Schema(description = "数据列")
    private List<DataColumn> dataColumns;
    @Schema(description = "数据")
    private List<T> data = new ArrayList<>();
}
