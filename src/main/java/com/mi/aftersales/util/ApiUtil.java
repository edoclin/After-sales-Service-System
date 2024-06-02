package com.mi.aftersales.util;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.annotation.AnnotationUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ClassUtil;
import com.mi.aftersales.entity.Api;
import com.mi.aftersales.enums.entity.ApiMethodEnum;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class ApiUtil {

    public static void checkPermission(HttpServletRequest req) {
        StpUtil.checkPermission(CharSequenceUtil.format("{}-{}", req.getMethod().toUpperCase(), req.getRequestURI()));
    }

    public static List<Api> allApi(String targetPackage) {
        List<Api> res = new ArrayList<>();
        ClassUtil.scanPackage(targetPackage).forEach(clazz -> {
            if (AnnotationUtil.hasAnnotation(clazz, RequestMapping.class)) {
                String[] path = AnnotationUtil.getAnnotation(clazz, RequestMapping.class).value();
                for (Method method : clazz.getMethods()) {
                    Api api = new Api();
                    if (AnnotationUtil.hasAnnotation(method, GetMapping.class)) {
                        GetMapping req = AnnotationUtil.getAnnotation(method, GetMapping.class);
                        api.setUri(path[0] + req.path()[0]);
                        api.setMethod(ApiMethodEnum.GET);
                        api.setApiComment(AnnotationUtil.getAnnotation(method, Operation.class).summary());
                        res.add(api);
                    } else if (AnnotationUtil.hasAnnotation(method, PostMapping.class)) {
                        PostMapping req = AnnotationUtil.getAnnotation(method, PostMapping.class);
                        api.setUri(path[0] + req.path()[0]);
                        api.setMethod(ApiMethodEnum.POST);
                        api.setApiComment(AnnotationUtil.getAnnotation(method, Operation.class).summary());
                        res.add(api);
                    } else if (AnnotationUtil.hasAnnotation(method, PutMapping.class)) {
                        PutMapping req = AnnotationUtil.getAnnotation(method, PutMapping.class);
                        api.setUri(path[0] + req.path()[0]);
                        api.setMethod(ApiMethodEnum.PUT);
                        api.setApiComment(AnnotationUtil.getAnnotation(method, Operation.class).summary());
                        res.add(api);
                    } else if (AnnotationUtil.hasAnnotation(method, DeleteMapping.class)) {
                        DeleteMapping req = AnnotationUtil.getAnnotation(method, DeleteMapping.class);
                        api.setUri(path[0] + req.path()[0]);
                        api.setMethod(ApiMethodEnum.DELETE);
                        api.setApiComment(AnnotationUtil.getAnnotation(method, Operation.class).summary());
                        res.add(api);
                    } else if (AnnotationUtil.hasAnnotation(method, RequestMapping.class)) {
                        RequestMapping req = AnnotationUtil.getAnnotation(method, RequestMapping.class);
                        api.setUri(path[0] + req.value()[0]);
                        api.setMethod(ApiMethodEnum.valueOf(req.method()[0].name()));
                        api.setApiComment(AnnotationUtil.getAnnotation(method, Operation.class).summary());
                        res.add(api);
                    }
                }
            }
        });
        return res;
    }
}
