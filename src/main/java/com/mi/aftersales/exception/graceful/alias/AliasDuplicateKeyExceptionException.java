package com.mi.aftersales.exception.graceful.alias;

import com.feiniaojin.gracefulresponse.api.ExceptionAliasFor;
import org.springframework.dao.DuplicateKeyException;

/**
 * @description: 请求参数非法
 * @return:
 * @author: edoclin
 * @created: 2024/6/1 17:50
 **/
@ExceptionAliasFor(code = "-1", msg = "唯一约束冲突！", aliasFor = {DuplicateKeyException.class})
public class AliasDuplicateKeyExceptionException extends RuntimeException {
}