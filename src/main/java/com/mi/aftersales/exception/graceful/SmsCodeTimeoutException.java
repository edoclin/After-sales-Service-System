package com.mi.aftersales.exception.graceful;

import com.feiniaojin.gracefulresponse.api.ExceptionMapper;

/**
 * @description: 非法的临时令牌
 * @return:
 * @author: edoclin
 * @created: 2024/5/12 12:33
 **/
@ExceptionMapper(code = "6", msg = "验证码已过期")
public class SmsCodeTimeoutException extends RuntimeException {
}
