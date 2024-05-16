package com.mi.aftersales.config;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ClassUtil;
import com.mi.aftersales.config.yaml.bean.InitConfig;
import com.mi.aftersales.controller.PlaceholderController;
import com.mi.aftersales.entity.*;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import com.mi.aftersales.service.*;
import com.mi.aftersales.util.ApiUtil;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.scheduling.annotation.EnableAsync;

import javax.annotation.Resource;
import java.util.ArrayList;

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
    private IApiService iApiService;

    @Resource
    private InitConfig initConfig;

    @Resource
    private ILoginService iLoginService;

    @Resource
    private IPermissionService iPermissionService;

    @Resource
    private IMiddlePermissionApiService iMiddlePermissionApiService;

    @Resource
    private IMiddleLoginPermissionService iMiddleLoginPermissionService;

    /**
     * @description: 缓存API列表
     * @return:
     * @author: edoclin
     * @created: 2024/5/16 21:23
     **/
    public void cachedApis() {

        redisTemplate.delete("apis:cached");
        ArrayList<String> apis = new ArrayList<>();
        iApiService.list().forEach(api -> apis.add(CharSequenceUtil.format("{}-{}", api.getMethod().name().toUpperCase(), api.getUri())));
        redisTemplate.opsForList().leftPushAll("apis:cached", apis);
    }

    /**
     * @description: 更新API列表
     * @return:
     * @author: edoclin
     * @created: 2024/5/16 21:23
     **/
    public void initApiTable() {
        ApiUtil.allApi(ClassUtil.getPackage(PlaceholderController.class)).forEach(api -> {
            Api one = iApiService.lambdaQuery().eq(Api::getMethod, api.getMethod()).eq(Api::getUri, api.getUri()).one();
            if (BeanUtil.isEmpty(one)) {
                one = new Api();
            }
            BeanUtil.copyProperties(api, one, CopyOptions.create().setIgnoreNullValue(true));
            iApiService.saveOrUpdate(one);
        });
    }


    private void init() {
        Permission permission = iPermissionService.lambdaQuery().eq(Permission::getPermissionKey, initConfig.getPermissionKey()).eq(Permission::getPermissionName, initConfig.getPermissionName()).one();
        // 创建默认权限
        if (BeanUtil.isEmpty(permission)) {
            permission = new Permission();
            permission.setPermissionKey(initConfig.getPermissionKey());
            permission.setPermissionName(initConfig.getPermissionName());
            iPermissionService.save(permission);
        }

        Permission finalPermission = permission;
        // 默认权限管理所有API
        iApiService.list().forEach(api -> {
            MiddlePermissionApi one = iMiddlePermissionApiService.lambdaQuery().eq(MiddlePermissionApi::getPermissionId, finalPermission.getPermissionId()).eq(MiddlePermissionApi::getApiId, api.getApiId()).one();
            if (BeanUtil.isEmpty(one)) {
                one = new MiddlePermissionApi();
                one.setPermissionId(finalPermission.getPermissionId());
                one.setApiId(api.getApiId());
                iMiddlePermissionApiService.save(one);
            }
        });

        Login login = iLoginService.lambdaQuery().eq(Login::getMobile, initConfig.getLoginMobile()).one();
        // 创建默认用户
        if (BeanUtil.isEmpty(login)) {
            login = new Login();
            login.setMobile(initConfig.getLoginMobile());
            login.setLoginType(LoginTypeEnum.EMPLOYEE);
            iLoginService.save(login);
        }

        MiddleLoginPermission one = iMiddleLoginPermissionService.lambdaQuery().eq(MiddleLoginPermission::getLoginId, initConfig.getLoginMobile()).eq(MiddleLoginPermission::getPermissionId, finalPermission.getPermissionId()).one();

        if (BeanUtil.isEmpty(one)) {
            one = new MiddleLoginPermission();
            one.setLoginId(login.getLoginId());
            one.setPermissionId(finalPermission.getPermissionId());
            iMiddleLoginPermissionService.save(one);
        }
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        RLock fairLock = redissonClient.getFairLock("init");
        if (fairLock.tryLock()) {
            initApiTable();
            cachedApis();
            init();

            fairLock.unlock();
        }
    }
}
