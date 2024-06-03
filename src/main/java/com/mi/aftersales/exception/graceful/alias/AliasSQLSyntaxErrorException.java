package com.mi.aftersales.exception.graceful.alias;

import com.fasterxml.jackson.databind.exc.InvalidFormatException;
import com.feiniaojin.gracefulresponse.api.ExceptionAliasFor;
import org.springframework.http.converter.HttpMessageNotReadableException;

import java.sql.SQLSyntaxErrorException;

/**
 * @description: 请求参数非法
 * @return:
 * @author: edoclin
 * @created: 2024/6/1 17:50
 **/
@ExceptionAliasFor(code = "-1", msg = "非法的查询语句！", aliasFor = {SQLSyntaxErrorException.class})
public class AliasSQLSyntaxErrorException extends RuntimeException {
}