package com.mi.aftersales.exception.graceful;

import com.feiniaojin.gracefulresponse.api.ExceptionMapper;

/**
 * @description: 用户未登录异常
 * @return:
 * @author: edoclin
 * @created: 2024/5/12 12:33
 **/
@ExceptionMapper(code = "-1", msg = "非法的物料Id！")
public class IllegalMaterialIdException extends RuntimeException {
}
