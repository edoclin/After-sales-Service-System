package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.mi.aftersales.entity.SmsLog;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.SmsLogVo;
import com.mi.aftersales.repository.ISmsLogRepository;
import com.mi.aftersales.service.SmsLogService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 短信推送日志 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SmsLogServiceImpl implements SmsLogService {

    @Resource
    private ISmsLogRepository iSmsLogRepository;


    @Override
    public PageResult<SmsLogVo> listSmsLog(ConditionQuery query) {

        PageResult<SmsLogVo> result = new PageResult<>();

        QueryWrapper<SmsLog> wrapper = QueryUtil.buildWrapper(query, SmsLog.class);

        result.setTotal(iSmsLogRepository.count(wrapper));

        iSmsLogRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(smsLog -> {
            SmsLogVo item = new SmsLogVo();
            BeanUtil.copyProperties(smsLog, item, DateUtil.copyDate2yyyyMMddHHmm());


            item.setSmsType(smsLog.getSmsType().getDesc());


        });
        return result;

    }
}
