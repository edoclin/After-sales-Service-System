package com.mi.aftersales.aspect;


import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.mi.aftersales.exception.graceful.NotAllowedAccessException;
import com.mi.aftersales.exception.graceful.NotLoginException;
import io.netty.util.CharsetUtil;
import lombok.val;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

import javax.servlet.http.HttpServletRequest;

/**
 * @description:
 * @author: edoclin
 * @created: 2024/5/16 22:10
 **/
@Aspect
@Component
public class CheckPermissionAspect {

    @Pointcut("@annotation(com.mi.aftersales.aspect.anno.CheckPermission)")
    public void permission() {
    }

    @Around("permission()")
    public Object aroundAdvice(ProceedingJoinPoint proceedingJoinPoint) throws Throwable {
        if (!StpUtil.isLogin()) {
            throw new NotLoginException();
        }
        RequestAttributes attributes = RequestContextHolder.getRequestAttributes();

        if (BeanUtil.isEmpty(attributes)) {
            throw new NotAllowedAccessException();
        }
        HttpServletRequest request = (HttpServletRequest) attributes.resolveReference(RequestAttributes.REFERENCE_REQUEST);

        if (BeanUtil.isEmpty(request)) {
            throw new NotAllowedAccessException();
        }

        String permission = CharSequenceUtil.format("{}-{}", request.getMethod().toUpperCase(), request.getRequestURI());

        if (!StpUtil.hasPermission(permission)) {
            throw new NotAllowedAccessException();
        }
        return proceedingJoinPoint.proceed();
    }
}
