package com.mi.aftersales.util.query;

import cn.hutool.core.annotation.AnnotationUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.ReflectUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.exception.graceful.alias.AliasInvalidFormatException;
import com.mi.aftersales.util.query.enums.Predicate;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Set;

/**
 * @description: 高级条件查询
 * @return:
 * @author: edoclin
 * @created: 2024/5/17 21:31
 **/

public class QueryUtil {

    private QueryUtil() {

    }

    public static <T> Set<String> columns(Class<T> clazz) {
        HashSet<String> res = new HashSet<>();
        for (Field field : ReflectUtil.getFields(clazz)) {
            if (AnnotationUtil.hasAnnotation(field, EnableQuery.class)) {
                res.add(field.getName());
            }
        }
        return res;
    }

    public static <T> QueryWrapper<T> buildEmptyQueryWrapper(Class<T> clazz) {
        return Wrappers.query(clazz).checkSqlInjection();
    }

    public static <T> QueryWrapper<T> buildWrapper(ConditionQuery query, Class<T> clazz) {
        Set<String> columns = columns(clazz);

        // 开启SQL防注入检查
        QueryWrapper<T> wrapper = Wrappers.query(clazz).checkSqlInjection();

        for (QueryParam queryParam : query.getParams()) {
            if (!columns.contains(queryParam.getColumn())) {
                throw new GracefulResponseException(CharSequenceUtil.format("参数({})不支持条件查询", queryParam.getColumn()));
            }

            if (queryParam.getPredicate() == Predicate.OR) {
                wrapper = wrapper.or();
            }

            String underlineColumn = CharSequenceUtil.toUnderlineCase(queryParam.getColumn());

            switch (queryParam.getOperator()) {
                case STR_EQ, NUM_EQ, BOOL_EQ -> {
                    if (CharSequenceUtil.isNotBlank(queryParam.getValue())) {
                        wrapper = wrapper.eq(underlineColumn, queryParam.getValue());
                    }
                }
                case STR_LIKE -> {
                    if (CharSequenceUtil.isNotBlank(queryParam.getValue())) {
                        wrapper = wrapper.like(underlineColumn, queryParam.getValue());
                    }
                }
                case DATE_RANGE -> {
                    if (ObjectUtil.isNotNull(queryParam.getLeft()) && ObjectUtil.isNotNull(queryParam.getRight())) {
                        wrapper = wrapper.between(underlineColumn, queryParam.getLeft(), queryParam.getRight());
                    }
                }
                case NUM_GT -> {
                    if (CharSequenceUtil.isNotBlank(queryParam.getValue())) {
                        wrapper = wrapper.gt(underlineColumn, queryParam.getValue());
                    }
                }
                case NUM_GE -> {
                    if (CharSequenceUtil.isNotBlank(queryParam.getValue())) {
                        wrapper = wrapper.ge(underlineColumn, queryParam.getValue());
                    }
                }
                case NUM_LT -> {
                    if (CharSequenceUtil.isNotBlank(queryParam.getValue())) {
                        wrapper = wrapper.lt(underlineColumn, queryParam.getValue());
                    }
                }
                case NUM_LE -> {
                    if (CharSequenceUtil.isNotBlank(queryParam.getValue())) {
                        wrapper = wrapper.le(underlineColumn, queryParam.getValue());
                    }
                }
                default -> throw new AliasInvalidFormatException();
            }
            switch (queryParam.getOrderBy()) {
                case ASC -> wrapper = wrapper.orderByAsc(underlineColumn);
                case DESC -> wrapper = wrapper.orderByDesc(underlineColumn);
                default -> throw new AliasInvalidFormatException();
            }
        }
        return wrapper;
    }

}
