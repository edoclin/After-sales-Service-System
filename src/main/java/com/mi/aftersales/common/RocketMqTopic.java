package com.mi.aftersales.common;

import org.springframework.stereotype.Service;

/**
 * @description: rocketMq主题
 * @return:
 * @author: edoclin
 * @created: 2024/5/19 18:19
 **/
@Service
public class RocketMqTopic {
    public static final String ROCKETMQ_TOPIC_4_ORDER_LOG = "order-log-topic";
    public static final String ROCKETMQ_TOPIC_4_ALIPAY_ORDER = "alipay-order-topic";
    public static final String ROCKETMQ_TOPIC_4_SMS = "sms-topic";
    public static final String ROCKETMQ_TOPIC_4_ORDER_UPLOAD = "order-upload-topic";
    public static final String ROCKETMQ_TOPIC_4_ORDER_MATERIAL = "order-material-topic";
    public static final String ROCKETMQ_TOPIC_4_MATERIAL_LOG = "material-log-topic";

    private RocketMqTopic() {}
}
