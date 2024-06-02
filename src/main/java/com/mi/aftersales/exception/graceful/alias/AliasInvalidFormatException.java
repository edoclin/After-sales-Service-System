package com.mi.aftersales.exception.graceful.alias;

import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import com.feiniaojin.gracefulresponse.api.ExceptionAliasFor;
import org.springframework.http.converter.HttpMessageNotReadableException;

/**
 * @description: 请求参数非法
 * @return:
 * @author: edoclin
 * @created: 2024/6/1 17:50
 **/
@ExceptionAliasFor(code = "1", msg = "请求参数非法！", aliasFor = {InvalidFormatException.class, HttpMessageNotReadableException.class})
public class AliasInvalidFormatException extends RuntimeException {
}