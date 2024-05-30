package com.mi.aftersales.exception.graceful;

import com.feiniaojin.gracefulresponse.api.ExceptionMapper;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/12 12:33
 **/
@ExceptionMapper(code = "-1", msg = "服务器繁忙")
public class ServerErrorException extends RuntimeException {
    public ServerErrorException() {
        super();
    }
    public ServerErrorException(String message) {
        super(message);
    }
}
