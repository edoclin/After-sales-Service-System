package com.mi.aftersales.config.rocketmq;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import com.mi.aftersales.entity.OrderUpload;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.repository.IOrderUploadRepository;
import com.mi.aftersales.vo.message.OrderUploadMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import java.util.ArrayList;
import java.util.List;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_UPLOAD;

/**
 * @description: 工单文件上传
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 18:24
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = ROCKETMQ_TOPIC_4_ORDER_UPLOAD, consumerGroup = ROCKETMQ_TOPIC_4_ORDER_UPLOAD)
public class OrderUploadConsumer implements RocketMQListener<List<OrderUpload>> {

    @Resource
    private IOrderUploadRepository iOrderUploadRepository;

    @Resource
    private IFileRepository iFileRepository;


    @Override
    public void onMessage(List<OrderUpload> batch) {
        try {
            iOrderUploadRepository.saveBatch(batch);
            log.info(CharSequenceUtil.format("工单文件上传消费成功！（{}）", JSONUtil.toJsonStr(batch)));
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
    }
}
