package com.mi.aftersales.exception.graceful.alias;

import com.feiniaojin.gracefulresponse.api.ExceptionMapper;
import com.mi.aftersales.exception.graceful.BaseCustomException;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/12 12:33
 **/
@ExceptionMapper(code = "-1", msg = "非法的商品属性Id！")
public class IllegalSkuAttrIdException extends BaseCustomException {
}
