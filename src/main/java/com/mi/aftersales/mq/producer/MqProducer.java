package com.mi.aftersales.mq.producer;

import cn.hutool.json.JSONUtil;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @description: 消息生产者
 * @return:
 * @author: edoclin
 * @created: 2024/6/5 21:35
 **/
@Component
public class MqProducer {

    @Resource
    private RocketMQTemplate rocketmqTemplate;

    private final SendCallback sendCallback = new RocketMqSendCallback();

    public SendResult syncSend(String topic, Object payload) {
        return rocketmqTemplate.syncSend(topic, MessageBuilder.withPayload(payload).build());
    }

    public void asyncSend(String topic, Object payload) {
        rocketmqTemplate.asyncSend(topic, MessageBuilder.withPayload(payload).build(), sendCallback);
    }
}

class RocketMqSendCallback implements SendCallback {
    private static final Logger log = LoggerFactory.getLogger("MqProducerLog");
    @Override
    public void onSuccess(SendResult sendResult) {
        log.info(SendResult.encoderSendResultToJson(sendResult));
    }

    @Override
    public void onException(Throwable throwable) {
        log.error(throwable.getMessage());
    }
}