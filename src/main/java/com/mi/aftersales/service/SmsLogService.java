package com.mi.aftersales.service;

import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.SmsLogVo;
import com.mi.aftersales.util.query.ConditionQuery;

import java.util.List;

/**
 * <p>
 * 短信推送日志 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface SmsLogService {

    PageResult<SmsLogVo> listSmsLog(ConditionQuery query);
}
