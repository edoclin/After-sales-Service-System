package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.SaTokenInfo;
import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.exception.graceful.ExampleException;
import com.mi.aftersales.exception.graceful.NotLoginException;
import com.mi.aftersales.service.ILoginService;
import com.mi.aftersales.vo.LoginVo;
import com.mi.aftersales.vo.form.LoginBySmsForm;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 登录表 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/login")
public class LoginController {
    @Resource
    private ILoginService iLoginService;


    /**
     * @description: 验证码登录示例, @Valid表单一定要验证!!!
     * 规则定义在LoginBySmsForm中
     * @return:
     * @author: edoclin
     * @created: 2024/5/14 14:06
     **/
    @PostMapping("/")
    @ApiOperation("示例: 验证码登录接口")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "mobile", value = "用户注册的手机号", defaultValue = "13111111111", required = true),
            @ApiImplicitParam(name = "code", value = "用户接收到的短信验证码", defaultValue = "ABCDEF", required = true)})
    public LoginVo example1(@RequestBody @Valid LoginBySmsForm form) {
        LoginVo loginVo = new LoginVo();

        // 登录校验
        if (StpUtil.isLogin()) {
            // pass
        }
        // 模拟查询到的loginId登录
        StpUtil.login("loginId");


        // 填充信息
        loginVo.setTokenName(StpUtil.getTokenName()).setTokenValue(StpUtil.getTokenValue());

        // 直接返回vo实体

        /*
        * 响应结果示例
        * {
              "status": {
                "code": "1",
                "msg": "ok"
              },
              "payload": {
                "tokenName": "template-token",
                "tokenValue": "tG_IjzhO7C6RJZcY2_0HqqjWximP8c2wAn__"
              }
            }
        * */
        return loginVo;
    }

    @GetMapping("/exception")
    @ApiOperation("示例: 统一异常返回示例")
    public Object example2() {
        try {
            int a = 5 / 0;
        } catch (NotLoginException e) {
            throw new NotLoginException();
        } catch (RuntimeException e) {
            // 业务逻辑中遇到需要返回给用户失败信息时, 在exception.graceful中定义一个异常直接抛出
            throw new ExampleException();
        }
        return null;
        /*
        响应结果:
        {
          "status": {
            "code": "3",
            "msg": "样例异常"
          },
          "payload": {}
        }
        * */

    }
}
