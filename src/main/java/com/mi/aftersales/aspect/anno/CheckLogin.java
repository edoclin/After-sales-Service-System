package com.mi.aftersales.aspect.anno;


import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * @description: 检查登录
 * @return:
 * @author: edoclin
 * @created: 2024/5/16 13:43
 **/
@Target(ElementType.METHOD)
//@Retention(RetentionPolicy.RUNTIME)
public @interface CheckLogin {
}
