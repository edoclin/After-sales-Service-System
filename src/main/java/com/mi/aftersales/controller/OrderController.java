package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.entity.Fapiao;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import com.mi.aftersales.service.IFapiaoService;
import com.mi.aftersales.service.IOrderService;
import com.mi.aftersales.vo.form.ClientOrderForm;
import com.mi.aftersales.vo.form.LoginBySmsForm;
import com.mi.aftersales.vo.result.LoginResultVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.statemachine.StateMachine;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 工单 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/order")
public class OrderController {
    @Resource
    private StateMachine<OrderStatusEnum, OrderStatusChangeEventEnum> orderStateMachine;


    @Resource
    private IFapiaoService iFapiaoService;

    @Resource
    private IOrderService iOrderService;


    @PostMapping(path = "/")
    @Operation(summary = "客户创建工单", description = "客户创建工单")
    public void postOrder(@RequestBody @Valid ClientOrderForm form) {
        Fapiao byId = iFapiaoService.getById(form.getFapiaoId());

        if (BeanUtil.isEmpty(byId) || !CharSequenceUtil.equals(byId.getCreatedId(), StpUtil.getLoginIdAsString())) {
            throw new GracefulResponseException("非法的发票ID！");
        }

    }

    /*
     * 订单状态流程转换
     * */
    private Message<OrderStatusChangeEventEnum> statusFlow(OrderStatusChangeEventEnum payload, String orderId) {
        return MessageBuilder.withPayload(payload).setHeader("order-id", orderId).build();
    }

}
