package com.mi.aftersales.service.impl;

import com.mi.aftersales.entity.SmsLog;
import com.mi.aftersales.mapper.SmsLogMapper;
import com.mi.aftersales.service.ISmsLogService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 短信推送日志 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SmsLogServiceImpl extends ServiceImpl<SmsLogMapper, SmsLog> implements ISmsLogService {

}