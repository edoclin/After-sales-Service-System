package com.mi.aftersales.config;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.util.IdUtil;
import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import com.baomidou.mybatisplus.core.incrementer.IdentifierGenerator;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.LocalDateTime;

/**
 * @description: MyBatisPlus配置
 * @return:
 * @author: edoclin
 * @created: 2024/5/13 22:32
 **/
@Configuration
public class MyBatisPlusConfig implements MetaObjectHandler, IdentifierGenerator {
    /**
     * @description: 插入字段填充
     * @return:
     * @author: edoclin
     * @created: 2024/5/13 22:35
     **/
    @Override
    public void insertFill(MetaObject metaObject) {
        this.strictInsertFill(metaObject, "createdTime", LocalDateTime.class, LocalDateTime.now());
        if (StpUtil.isLogin()) {
            this.strictInsertFill(metaObject, "createdId", String.class, StpUtil.getLoginIdAsString());
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
        this.strictInsertFill(metaObject, "updatedTime", LocalDateTime.class, LocalDateTime.now());
        if (StpUtil.isLogin()) {
            this.strictInsertFill(metaObject, "updatedId", String.class, StpUtil.getLoginIdAsString());
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
    public Number nextId(Object entity) {
        // table ID生成策略
        return IdUtil.getSnowflakeNextId();
    }

    @Override
    public boolean assignId(Object idValue) {
        return IdentifierGenerator.super.assignId(idValue);
    }

    @Override
    public String nextUUID(Object entity) {
        return IdUtil.getSnowflakeNextIdStr();
    }
}
