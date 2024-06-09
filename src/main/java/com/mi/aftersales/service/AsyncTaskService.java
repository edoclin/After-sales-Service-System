package com.mi.aftersales.service;

import cn.dev33.satoken.session.SaSession;
import com.mi.aftersales.entity.Login;
import org.dromara.sms4j.api.entity.SmsResponse;
import org.springframework.scheduling.annotation.Async;

import java.util.List;
import java.util.concurrent.CompletableFuture;


/**
 * @description: 异步任务
 * @return:
 * @author: edoclin
 * @created: 2024/6/8 21:49
 **/
public interface AsyncTaskService {
    @Async("asyncPoolTaskExecutor")
    void listLoginPermissions(SaSession saSession, Login login);

    @Async("asyncPoolTaskExecutor")
    void mqAsyncSendMessage(String topic, Object payload);


    @Async("asyncPoolTaskExecutor")
    void asyncSmsLog(String mobile, SmsResponse smsResponse);
}
