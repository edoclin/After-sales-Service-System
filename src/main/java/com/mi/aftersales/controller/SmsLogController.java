package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.enums.entity.EmployeeRoleEnum;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.OrderStatusLogVo;
import com.mi.aftersales.pojo.vo.SmsLogVo;
import com.mi.aftersales.service.SmsLogService;
import com.mi.aftersales.util.query.ConditionQuery;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 短信推送日志 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/smsLog")
public class SmsLogController {
    @Resource
    private SmsLogService smsLogService;


    @GetMapping(path = "/")
    @Operation(summary = "查询短信发送日志", description = "查询短信发送日志")
    @CheckLogin
    public PageResult<SmsLogVo> listSmsLogByCondition(@RequestBody @Valid ConditionQuery query) {
        StpUtil.checkRole(EmployeeRoleEnum.SYSTEM_MANAGER.name());
        return smsLogService.listSmsLog(query);
    }
}
