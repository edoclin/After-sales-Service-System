package com.mi.aftersales.mq.consumer;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import com.mi.aftersales.config.yaml.bean.CustomSmsConfig;
import com.mi.aftersales.entity.SmsLog;
import com.mi.aftersales.enums.controller.SmsType;
import com.mi.aftersales.entity.Login;
import com.mi.aftersales.entity.Order;
import com.mi.aftersales.enums.entity.SmsResultEnum;
import com.mi.aftersales.enums.entity.SmsTypeEnum;
import com.mi.aftersales.exception.graceful.IllegalLoginIdException;
import com.mi.aftersales.exception.graceful.IllegalOrderIdException;
import com.mi.aftersales.repository.ILoginRepository;
import com.mi.aftersales.repository.IOrderRepository;
import com.mi.aftersales.repository.ISmsLogRepository;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.dromara.sms4j.api.SmsBlend;
import org.dromara.sms4j.api.entity.SmsResponse;
import org.dromara.sms4j.core.factory.SmsFactory;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import static com.mi.aftersales.common.RocketMqTopic.ROCKETMQ_TOPIC_4_SMS;

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
    private ILoginRepository iLoginRepository;


    @Resource
    private IOrderRepository iOrderRepository;

    @Resource
    private CustomSmsConfig customSmsConfig;

    @Resource
    private ISmsLogRepository iSmsLogRepository;

    @Override
    public void onMessage(String orderId) {

        if (Boolean.TRUE.equals(customSmsConfig.getEnable())) {
            Order order = iOrderRepository.getById(orderId);

            if (BeanUtil.isEmpty(order)) {
                throw new IllegalOrderIdException();
            }

            Login login = iLoginRepository.getById(order.getClientLoginId());

            if (BeanUtil.isEmpty(login)) {
                throw new IllegalLoginIdException();
            }
            SmsBlend smsBlend = SmsFactory.getSmsBlend(SmsType.ORDER_NOTIFY.getValue());
            SmsResponse smsResponse = smsBlend.sendMessage(login.getMobile(), orderId.substring(orderId.length() - 6));
            log.info(CharSequenceUtil.format("工单状态提醒短信发送成功（{}）", login.getMobile()));

            SmsLog smsLog = new SmsLog();

            smsLog.setSmsType(SmsTypeEnum.ORDER)
                    .setDetail(JSONUtil.toJsonStr(smsResponse))
                    .setMobile(login.getMobile())
                    .setResult(smsResponse.isSuccess() ? SmsResultEnum.SUCCESS : SmsResultEnum.FAIL)
                    .setResponse(JSONUtil.toJsonStr(smsResponse));

            iSmsLogRepository.save(smsLog);
        } else {
            log.warn(CharSequenceUtil.format("当前短信未激活状态（enable is false）"));
        }

    }
}
