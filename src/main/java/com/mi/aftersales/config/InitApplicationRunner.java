package com.mi.aftersales.config;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ClassUtil;
import com.mi.aftersales.common.yaml.bean.InitConfig;
import com.mi.aftersales.controller.PlaceholderController;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.enums.entity.LoginTypeEnum;
import com.mi.aftersales.exception.graceful.TransactionalErrorException;
import com.mi.aftersales.repository.*;
import com.mi.aftersales.util.ApiUtil;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;

import static com.mi.aftersales.common.RedisNamespace.API_CACHE_PREFIX;

/**
 * @description: 启动初始化配置
 * @return:
 * @author: edoclin
 * @created: 2024/5/16 19:28
 **/
@Configuration
@EnableAsync
public class InitApplicationRunner implements ApplicationRunner {

    @Resource
    private RedisTemplate<String, String> redisTemplate;

    @Resource
    private RedissonClient redissonClient;

    @Resource
    private IApiRepository iApiRepository;

    @Resource
    private InitConfig initConfig;

    @Resource
    private ILoginRepository iLoginRepository;

    @Resource
    private IPermissionRepository iPermissionRepository;

    @Resource
    private IPermissionApiRepository iPermissionApiRepository;

    @Resource
    private ILoginPermissionRepository iLoginPermissionRepository;

    @Resource
    private ILoginRoleRepository iLoginRoleRepository;

    /**
     * @description: 缓存API列表
     * @return:
     * @author: edoclin
     * @created: 2024/5/16 21:23
     **/
    public void cachedApis() {
        redisTemplate.delete(API_CACHE_PREFIX);
        ArrayList<String> apis = new ArrayList<>();
        iApiRepository.list().forEach(api -> apis.add(CharSequenceUtil.format("{}-{}", api.getMethod().name().toUpperCase(), api.getUri())));
        redisTemplate.opsForList().leftPushAll(API_CACHE_PREFIX, apis);
    }

    /**
     * @description: 更新API列表
     * @return:
     * @author: edoclin
     * @created: 2024/5/16 21:23
     **/
    public void initApiTable() {
        ApiUtil.allApi(ClassUtil.getPackage(PlaceholderController.class)).forEach(api -> {
            Api one = iApiRepository.lambdaQuery().eq(Api::getMethod, api.getMethod()).eq(Api::getUri, api.getUri()).one();
            if (BeanUtil.isEmpty(one)) {
                one = new Api();
            }
            BeanUtil.copyProperties(api, one, CopyOptions.create().setIgnoreNullValue(true));
            iApiRepository.saveOrUpdate(one);
        });
    }


    private void init() {

        // 用户初始化
        Permission permission = iPermissionRepository.lambdaQuery().eq(Permission::getPermissionKey, initConfig.getPermissionKey()).eq(Permission::getPermissionName, initConfig.getPermissionName()).one();
        // 创建默认权限
        if (BeanUtil.isEmpty(permission)) {
            permission = new Permission();
            permission.setPermissionKey(initConfig.getPermissionKey());
            permission.setPermissionName(initConfig.getPermissionName());
            iPermissionRepository.save(permission);
        }

        Permission finalPermission = permission;
        // 默认权限管理所有API
        iApiRepository.list().forEach(api -> {
            PermissionApi one = iPermissionApiRepository.lambdaQuery().eq(PermissionApi::getPermissionId, finalPermission.getPermissionId()).eq(PermissionApi::getApiId, api.getApiId()).one();
            if (BeanUtil.isEmpty(one)) {
                one = new PermissionApi();
                one.setPermissionId(finalPermission.getPermissionId());
                one.setApiId(api.getApiId());
                iPermissionApiRepository.save(one);
            }
        });

        Login login = iLoginRepository.lambdaQuery().eq(Login::getMobile, initConfig.getLoginMobile()).one();
        // 创建默认用户
        if (BeanUtil.isEmpty(login)) {
            login = new Login();
            login.setMobile(initConfig.getLoginMobile());
            login.setLoginType(LoginTypeEnum.EMPLOYEE);
            iLoginRepository.save(login);
        }

        LoginPermission one = iLoginPermissionRepository.lambdaQuery().eq(LoginPermission::getLoginId, initConfig.getLoginMobile()).eq(LoginPermission::getPermissionId, finalPermission.getPermissionId()).one();

        if (BeanUtil.isEmpty(one)) {
            one = new LoginPermission();
            one.setLoginId(login.getLoginId());
            one.setPermissionId(finalPermission.getPermissionId());
            iLoginPermissionRepository.save(one);
        }

        if (CollUtil.isEmpty(iLoginRoleRepository.lambdaQuery().eq(LoginRole::getLoginId, login.getLoginId()).list())) {
            String loginId = login.getLoginId();
            initConfig.getRoleList().forEach(role -> {
                LoginRole loginRole = new LoginRole();
                loginRole.setLoginId(loginId)
                        .setCreatedId(loginId)
                        .setEmployeeRole(role);
                iLoginRoleRepository.save(loginRole);
            });
        }


    }

    @Override
    @Transactional(rollbackFor = TransactionalErrorException.class)
    public void run(ApplicationArguments args) {
        RLock fairLock = redissonClient.getFairLock("init");
        if (fairLock.tryLock()) {
//            initApiTable();
//            cachedApis();
//            init();
            fairLock.unlock();
        }
    }
}
