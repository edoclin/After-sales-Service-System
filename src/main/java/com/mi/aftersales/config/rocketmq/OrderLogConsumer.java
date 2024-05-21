package com.mi.aftersales.config.rocketmq;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.service.IOrderStatusLogService;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_LOG;

/**
 * @description: 工单状态日志消费
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 18:24
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = ROCKETMQ_TOPIC_4_ORDER_LOG, consumerGroup = "aftersales_consumer_group")
public class OrderLogConsumer implements RocketMQListener<OrderStatusLog> {

    @Resource
    private IOrderStatusLogService iOrderStatusLogService;

    @Override
    public void onMessage(OrderStatusLog orderStatusLog) {
        if (BeanUtil.isNotEmpty(orderStatusLog)) {
            iOrderStatusLogService.save(orderStatusLog);
            log.info(CharSequenceUtil.format("工单状态日志消费成功！（{}）", JSONUtil.toJsonStr(orderStatusLog)));
        }
    }
}
