package com.mi.aftersales.exception.graceful;

import com.feiniaojin.gracefulresponse.api.ExceptionAliasFor;
import org.springframework.web.servlet.NoHandlerFoundException;

/**
 * @description: 接口不存在
 * @return:
 * @author: edoclin
 * @created: 2024/6/1 17:50
 **/
@ExceptionAliasFor(code = "1", msg = "接口不存在！", aliasFor = NoHandlerFoundException.class)
public class UrlNotFoundException extends RuntimeException {
}