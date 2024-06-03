package com.mi.aftersales.util.view.anno;


import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @description: 前端表格列的标签
 * @return:
 * @author: edoclin
 * @created: 2024/6/3 17:24
 **/
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface View {
    int index() default 256;

    String label() default "";

    boolean fixed() default false;

    boolean sortable() default false;
}
