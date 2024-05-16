package com.mi.aftersales.aspect;


import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.exception.graceful.NotLoginException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class CheckLoginAspect {

    @Pointcut("@annotation(com.mi.aftersales.aspect.anno.CheckLogin)")
    public void permission() {
    }

    @Around("permission()")
    public Object aroundAdvice(ProceedingJoinPoint proceedingJoinPoint) throws Throwable {
        if (!StpUtil.isLogin()) {
            throw new NotLoginException();
        }
        return proceedingJoinPoint.proceed();
    }
}
