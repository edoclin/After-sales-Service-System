package com.mi.aftersales.config;

import com.baomidou.mybatisplus.extension.plugins.inner.DataChangeRecorderInnerInterceptor;

/**
 * @description: 数据变动记录插件
 * @return:
 * @author: edoclin
 * @created: 2024/5/13 22:30
 **/
public class MyBatisPlusDataChangeRecorderInnerInterceptor extends DataChangeRecorderInnerInterceptor {
    @Override
    protected void dealOperationResult(OperationResult operationResult) {
        super.dealOperationResult(operationResult);
    }
}
