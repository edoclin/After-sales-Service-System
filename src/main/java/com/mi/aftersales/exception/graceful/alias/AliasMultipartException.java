package com.mi.aftersales.exception.graceful.alias;

import com.feiniaojin.gracefulresponse.api.ExceptionAliasFor;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.multipart.MultipartException;

/**
 * @description: 请求参数非法
 * @return:
 * @author: edoclin
 * @created: 2024/6/1 17:50
 **/
@ExceptionAliasFor(code = "-1", msg = "文件上传服务异常！", aliasFor = {MultipartException.class})
public class AliasMultipartException extends RuntimeException {
}