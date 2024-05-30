package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.SmsLog;
import com.mi.aftersales.mapper.SmsLogMapper;
import com.mi.aftersales.repository.ISmsLogRepository;
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
public class ISmsLogRepositoryImpl extends ServiceImpl<SmsLogMapper, SmsLog> implements ISmsLogRepository {

}
