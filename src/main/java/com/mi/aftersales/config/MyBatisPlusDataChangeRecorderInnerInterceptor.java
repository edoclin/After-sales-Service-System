package com.mi.aftersales.config;

import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.extension.plugins.inner.DataChangeRecorderInnerInterceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @description: 数据变动记录插件
 * @return:
 * @author: edoclin
 * @created: 2024/5/13 22:30
 **/
public class MyBatisPlusDataChangeRecorderInnerInterceptor extends DataChangeRecorderInnerInterceptor {
    private static final Logger log = LoggerFactory.getLogger("DataChangeRecorderLog");

    @Override
    protected void dealOperationResult(OperationResult operationResult) {
        log.info(JSONUtil.toJsonStr(operationResult));
    }
}
