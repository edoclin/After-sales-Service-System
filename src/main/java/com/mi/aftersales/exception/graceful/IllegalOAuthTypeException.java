package com.mi.aftersales.exception.graceful;

import com.feiniaojin.gracefulresponse.api.ExceptionMapper;

/**
 * @description: 不支持的登录方式异常
 * @return:
 * @author: edoclin
 * @created: 2024/5/12 12:33
 **/
@ExceptionMapper(code = "4", msg = "不支持的登录方式")
public class IllegalOAuthTypeException extends RuntimeException {
}
