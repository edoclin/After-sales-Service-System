package com.mi.aftersales.config.rocketmq;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.mi.aftersales.entity.MiddleOrderMaterial;
import com.mi.aftersales.service.iservice.IMiddleOrderMaterialService;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import java.util.List;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_MATERIAL;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 18:24
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = ROCKETMQ_TOPIC_4_ORDER_MATERIAL, consumerGroup = ROCKETMQ_TOPIC_4_ORDER_MATERIAL)
public class OrderMaterialConsumer implements RocketMQListener<List<MiddleOrderMaterial>> {

    @Resource
    private IMiddleOrderMaterialService iMiddleOrderMaterialService;

    @Override
    public void onMessage(List<MiddleOrderMaterial> batch) {
        if (CollUtil.isNotEmpty(batch)) {
            iMiddleOrderMaterialService.saveBatch(batch);
            log.info(CharSequenceUtil.format("工单物料申请（待申请）消息消费成功！"));
        }
    }
}
