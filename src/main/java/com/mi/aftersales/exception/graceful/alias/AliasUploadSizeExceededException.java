package com.mi.aftersales.exception.graceful.alias;

import com.feiniaojin.gracefulresponse.api.ExceptionAliasFor;
import org.springframework.web.multipart.MaxUploadSizeExceededException;

/**
 * @description: 接口不存在
 * @return:
 * @author: edoclin
 * @created: 2024/6/1 17:50
 **/
@ExceptionAliasFor(code = "1", msg = "单个文件大小超出限制！", aliasFor = MaxUploadSizeExceededException.class)
public class AliasUploadSizeExceededException extends RuntimeException {
}