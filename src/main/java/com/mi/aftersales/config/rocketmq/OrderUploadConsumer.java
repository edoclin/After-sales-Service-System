package com.mi.aftersales.config.rocketmq;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import com.mi.aftersales.entity.OrderStatusLog;
import com.mi.aftersales.entity.OrderUpload;
import com.mi.aftersales.service.IFileService;
import com.mi.aftersales.service.IOrderStatusLogService;
import com.mi.aftersales.service.IOrderUploadService;
import com.mi.aftersales.vo.message.OrderUploadMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import java.util.ArrayList;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_ORDER_UPLOAD;

/**
 * @description: 工单文件上传
 * @return:
 * @author: edoclin
 * @created: 2024/5/18 18:24
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = ROCKETMQ_TOPIC_4_ORDER_UPLOAD, consumerGroup = "aftersales_consumer_group")
public class OrderUploadConsumer implements RocketMQListener<OrderUploadMessage> {

    @Resource
    private IOrderUploadService iOrderUploadService;

    @Resource
    private IFileService iFileService;


    @Override
    public void onMessage(OrderUploadMessage orderUploadMessage) {
        if (BeanUtil.isNotEmpty(orderUploadMessage)) {
            ArrayList<OrderUpload> batch = new ArrayList<>();
            for (String fileId : orderUploadMessage.getFileIds()) {
                if (BeanUtil.isNotEmpty(iFileService.getById(fileId))) {
                    OrderUpload orderUpload = new OrderUpload();
                    orderUpload.setOrderId(orderUploadMessage.getOrderId());
                    orderUpload.setFileId(fileId);
                    orderUpload.setUploaderType(orderUploadMessage.getUploaderType());
                    batch.add(orderUpload);
                }
            }
            try {
                iOrderUploadService.saveBatch(batch);
                log.info(CharSequenceUtil.format("工单文件上传消费成功！（{}）", orderUploadMessage.getOrderId()));
            } catch (Exception e) {
                log.error(e.getMessage(), e);
            }
        }
    }
}
