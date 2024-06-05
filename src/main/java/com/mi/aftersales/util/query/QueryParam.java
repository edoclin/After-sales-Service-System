package com.mi.aftersales.util.query;

import com.mi.aftersales.util.query.enums.Operator;
import com.mi.aftersales.util.query.enums.OrderBy;
import com.mi.aftersales.util.query.enums.Predicate;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

import java.time.LocalDateTime;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/17 21:36
 **/
@Getter
@Schema(title = "高级检索条件", description = "高级检索条件")
public class QueryParam {
    @Schema(description = "条件连接符（AND、OR）")
    private Predicate predicate = Predicate.AND;
    @Schema(description = "检索的列名")
    private String column = null;
    @Schema(description = "检索的值")
    private String value = null;
    @Schema(description = "between的左界")
    private LocalDateTime left = null;
    @Schema(description = "between的右界")
    private LocalDateTime right = null;
    @Schema(description = "操作符（EQ、Like...）")
    private Operator operator = Operator.STR_EQ;
    @Schema(description = "排序方法（DESC、ASC）")
    private OrderBy orderBy = OrderBy.NONE;
}
