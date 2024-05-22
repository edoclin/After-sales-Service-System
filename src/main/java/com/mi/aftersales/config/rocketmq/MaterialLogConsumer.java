package com.mi.aftersales.config.rocketmq;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.mi.aftersales.entity.MaterialLog;
import com.mi.aftersales.entity.MiddleOrderMaterial;
import com.mi.aftersales.service.IMaterialLogService;
import com.mi.aftersales.service.IMiddleOrderMaterialService;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_MATERIAL_LOG;
import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_MATERIAL;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 18:24
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = ROCKETMQ_TOPIC_4_MATERIAL_LOG, consumerGroup = "aftersales_consumer_group")
public class MaterialLogConsumer implements RocketMQListener<List<MaterialLog>> {

    @Resource
    private IMaterialLogService iMaterialLogService;

    @Override
    public void onMessage(List<MaterialLog> batch) {
        if (CollUtil.isNotEmpty(batch)) {
            iMaterialLogService.saveBatch(batch);
            log.info(CharSequenceUtil.format("物料库存日志消息消费成功！"));
        }
    }
}
