package com.mi.aftersales.service.impl;

import cn.dev33.satoken.session.SaSession;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.enums.entity.LoginTypeEnum;
import com.mi.aftersales.enums.entity.SmsTypeEnum;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.mq.producer.MqProducer;
import com.mi.aftersales.repository.*;
import com.mi.aftersales.service.AsyncTaskService;
import org.dromara.sms4j.api.entity.SmsResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/**
 * @author fengs
 */
@Service
public class AsyncTaskServiceImpl implements AsyncTaskService {


    private static final Logger log = LoggerFactory.getLogger(AsyncTaskServiceImpl.class);

    @Resource
    private IApiRepository iApiRepository;

    @Resource
    private ILoginPermissionRepository iLoginPermissionRepository;

    @Resource
    private IPermissionApiRepository iPermissionApiRepository;

    @Resource
    private ILoginRoleRepository iLoginRoleRepository;

    @Resource
    private MqProducer mqProducer;

    @Resource
    private ISmsLogRepository iSmsLogRepository;


    @Async("asyncPoolTaskExecutor")
    @Override
    public void listLoginPermissions(SaSession saSession, Login login) {

        ArrayList<String> roles = new ArrayList<>();

        // 默认具有CLIENT角色
        roles.add(LoginTypeEnum.CLIENT.name());

        iLoginRoleRepository.lambdaQuery().eq(LoginRole::getLoginId, login.getLoginId()).list().forEach(loginRole -> roles.add(loginRole.getEmployeeRole().name()));

        saSession.set(SaSession.ROLE_LIST, roles);

        Set<String> permissions = new HashSet<>();
        iLoginPermissionRepository.lambdaQuery().eq(LoginPermission::getLoginId, login.getLoginId()).list().forEach(loginPermission -> iPermissionApiRepository.lambdaQuery().eq(PermissionApi::getPermissionId, loginPermission.getPermissionId()).list().forEach(middlePermissionApi -> {
            Api api = iApiRepository.getById(middlePermissionApi.getApiId());

            if (BeanUtil.isNotEmpty(api)) {
                permissions.add(CharSequenceUtil.format("{}-{}", api.getMethod().name().toUpperCase(), api.getUri()));
            }
        }));

        saSession.set(SaSession.PERMISSION_LIST, permissions.stream().toList());

        log.info("异步任务：loginSetRolesAndPermissions（{}）执行完成！", login);
    }

    /**
     * @description: 使用自定义线程池异步发送消息
     * @return:
     * @author: edoclin
     * @created: 2024/6/8 22:36
     **/
    @Async("asyncPoolTaskExecutor")
    @Override
    public void mqAsyncSendMessage(String topic, Object payload) {
        try {
            mqProducer.syncSend(topic, payload);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            throw new ServerErrorException();
        }
    }

    /**
     * @description: 登录验证码日志
     * @return:
     * @author: edoclin
     * @created: 2024/6/9 18:00
     **/

    @Async("asyncPoolTaskExecutor")
    @Override
    public void asyncSmsLog(String mobile, SmsResponse smsResponse) {
        try {
            SmsLog smsLog = new SmsLog();
            smsLog.setMobile(mobile)
                    .setResponse(JSONUtil.toJsonStr(smsResponse))
                    .setSmsType(SmsTypeEnum.LOGIN);
            iSmsLogRepository.save(smsLog);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
            throw new ServerErrorException();
        }
    }
}
