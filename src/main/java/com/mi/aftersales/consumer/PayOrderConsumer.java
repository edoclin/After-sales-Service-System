package com.mi.aftersales.consumer;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import com.mi.aftersales.entity.PayOrder;
import com.mi.aftersales.repository.IPayOrderRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ALIPAY_ORDER;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 18:24
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = ROCKETMQ_TOPIC_4_ALIPAY_ORDER, consumerGroup = ROCKETMQ_TOPIC_4_ALIPAY_ORDER)
public class PayOrderConsumer implements RocketMQListener<PayOrder> {
    @Resource
    private IPayOrderRepository iPayOrderRepository;

    @Override
    public void onMessage(PayOrder payOrder) {
        if (BeanUtil.isNotEmpty(payOrder)) {
            iPayOrderRepository.save(payOrder);
            log.info(CharSequenceUtil.format("工单支付账单创建消费成功！（{}）", JSONUtil.toJsonStr(payOrder)));
        }
    }
}
