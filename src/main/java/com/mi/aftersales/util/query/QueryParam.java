package com.mi.aftersales.util.query;

import com.mi.aftersales.util.query.enums.Operator;
import com.mi.aftersales.util.query.enums.OrderBy;
import com.mi.aftersales.util.query.enums.Predicate;
import lombok.Getter;

import java.time.LocalDateTime;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/17 21:36
 **/
@Getter
public class QueryParam {
    private Predicate predicate = Predicate.AND;
    private String column = null;
    private String value = null;
    private LocalDateTime left = null;
    private LocalDateTime right = null;
    private Operator operator = Operator.STR_EQ;
    private OrderBy orderBy = OrderBy.NONE;
}
