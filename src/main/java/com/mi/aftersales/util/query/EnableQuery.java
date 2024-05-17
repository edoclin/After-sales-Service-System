package com.mi.aftersales.util.query;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @description: 开启高级条件查询注解
 * @return:
 * @author: edoclin
 * @created: 2024/5/17 21:38
 **/
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface EnableQuery {
}
