package com.mi.aftersales.config;

import com.feiniaojin.gracefulresponse.AbstractExceptionAliasRegisterConfig;
import com.feiniaojin.gracefulresponse.ExceptionAliasRegister;
import com.mi.aftersales.exception.graceful.UploadSizeExceededException;
import com.mi.aftersales.exception.graceful.UrlNotFoundException;
import org.springframework.context.annotation.Configuration;


/**
 * @description: 异常配置
 * @return:
 * @author: edoclin
 * @created: 2024/6/1 17:53
 **/
@Configuration
public class GracefulResponseConfig extends AbstractExceptionAliasRegisterConfig {

    @Override
    protected void registerAlias(ExceptionAliasRegister aliasRegister) {
        //注册异常别名
        aliasRegister.doRegisterExceptionAlias(UrlNotFoundException.class).doRegisterExceptionAlias(UploadSizeExceededException.class);
    }
}