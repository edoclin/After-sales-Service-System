package com.mi.aftersales.config.mp;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.util.IdUtil;
import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import com.baomidou.mybatisplus.core.incrementer.IdentifierGenerator;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import com.mi.aftersales.config.mp.interceptor.MyBatisPlusDataChangeRecorderInnerInterceptor;
import com.mi.aftersales.entity.*;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

/**
 * @description: MyBatisPlus配置
 * @return:
 * @author: edoclin
 * @created: 2024/5/13 22:32
 **/
@Component
public class MyBatisPlusConfig implements MetaObjectHandler, IdentifierGenerator {



    /**
     * @description: 排除自动填充**id的类
     * @return:
     * @author: edoclin
     * @created: 2024/6/3 22:44
     **/
    private boolean excludeFillEntity(MetaObject metaObject) {
        return
                (metaObject.getOriginalObject() instanceof Login) ||
                        (metaObject.getOriginalObject() instanceof MiddleLoginPermission) ||
                        (metaObject.getOriginalObject() instanceof Permission) ||
                        (metaObject.getOriginalObject() instanceof MiddlePermissionApi) ||
                        (metaObject.getOriginalObject() instanceof OrderStatusLog) ||
                        (metaObject.getOriginalObject() instanceof PayOrder) ||
                        (metaObject.getOriginalObject() instanceof OrderUpload) ||
                        (metaObject.getOriginalObject() instanceof MaterialLog) ||
                        (metaObject.getOriginalObject() instanceof SmsLog) ||
                        (metaObject.getOriginalObject() instanceof Api);
    }

    /**
     * @description: 插入字段填充
     * @return:
     * @author: edoclin
     * @created: 2024/5/13 22:35
     **/
    @Override
    public void insertFill(MetaObject metaObject) {
        setFieldValByName("createdTime", LocalDateTime.now(), metaObject);
        if (!excludeFillEntity(metaObject) && StpUtil.isLogin()) {
            setFieldValByName("createdId", StpUtil.getLoginIdAsString(), metaObject);
        }
    }

    /**
     * @description: 更新字段填充
     * @return:
     * @author: edoclin
     * @created: 2024/5/13 22:36
     **/
    @Override
    public void updateFill(MetaObject metaObject) {
        setFieldValByName("updatedTime", LocalDateTime.now(), metaObject);
        if (!excludeFillEntity(metaObject) && StpUtil.isLogin()) {
            setFieldValByName("updatedId", StpUtil.getLoginIdAsString(), metaObject);
        }
    }


    @Bean
    public MybatisPlusInterceptor mybatisPlusInterceptor() {
        MybatisPlusInterceptor interceptor = new MybatisPlusInterceptor();
        interceptor.addInnerInterceptor(new MyBatisPlusDataChangeRecorderInnerInterceptor());
        interceptor.addInnerInterceptor(new PaginationInnerInterceptor(DbType.MYSQL));
        return interceptor;
    }

    @Override
    public boolean assignId(Object idValue) {
        return IdentifierGenerator.super.assignId(idValue);
    }

    @Override
    public Number nextId(Object entity) {
        return IdUtil.getSnowflakeNextId();
    }

    @Override
    public String nextUUID(Object entity) {
        return IdUtil.getSnowflakeNextIdStr();
    }
}
