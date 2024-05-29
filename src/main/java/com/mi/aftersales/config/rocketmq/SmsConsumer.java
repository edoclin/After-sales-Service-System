package com.mi.aftersales.config.rocketmq;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.yaml.bean.CustomSmsConfig;
import com.mi.aftersales.controller.enums.SmsCodeType;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.service.iservice.ILoginService;
import com.mi.aftersales.service.iservice.IOrderService;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.dromara.sms4j.api.SmsBlend;
import org.dromara.sms4j.api.entity.SmsResponse;
import org.dromara.sms4j.core.factory.SmsFactory;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_SMS;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 18:24
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = ROCKETMQ_TOPIC_4_SMS, consumerGroup = ROCKETMQ_TOPIC_4_SMS)
public class SmsConsumer implements RocketMQListener<String> {

    @Resource
    private ILoginService iLoginService;


    @Resource
    private IOrderService iOrderService;

    @Resource
    private CustomSmsConfig customSmsConfig;

    @Override
    public void onMessage(String orderId) {

        if (Boolean.TRUE.equals(customSmsConfig.getEnable())) {
            Order order = iOrderService.getById(orderId);

            if (BeanUtil.isEmpty(order)) {
                throw new GracefulResponseException("非法工单！");
            }

            Login login = iLoginService.getById(order.getClientLoginId());
            if (BeanUtil.isEmpty(login)) {
                throw new GracefulResponseException("非法用户！");
            }
            SmsBlend smsBlend = SmsFactory.getSmsBlend(SmsCodeType.ORDER_NOTIFY.getValue());
            SmsResponse smsResponse = smsBlend.sendMessage(login.getMobile(), orderId.substring(orderId.length() - 6));
            log.info(CharSequenceUtil.format("工单状态提醒短信发送成功（{}）", login.getMobile()));
        } else {
            log.warn(CharSequenceUtil.format("当前短信未激活状态（enable is false）"));

        }

    }
}
