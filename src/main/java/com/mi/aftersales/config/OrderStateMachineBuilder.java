package com.mi.aftersales.config;

import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Role;
import org.springframework.statemachine.StateContext;
import org.springframework.statemachine.StateMachine;
import org.springframework.statemachine.action.Action;
import org.springframework.statemachine.config.EnableStateMachine;
import org.springframework.statemachine.config.EnableStateMachineFactory;
import org.springframework.statemachine.config.StateMachineBuilder;
import org.springframework.statemachine.config.StateMachineConfigurerAdapter;
import org.springframework.statemachine.config.builders.StateMachineStateConfigurer;
import org.springframework.statemachine.config.builders.StateMachineTransitionConfigurer;

import javax.annotation.Resource;
import java.util.EnumSet;

/**
 * @description: 工单状态机
 * @return:
 * @author: edoclin
 * @created: 2024/5/16 15:19
 **/
@Configuration
@EnableStateMachine
public class OrderStateMachineBuilder {

    public static final String MACHINE_ID = "orderMachine";

    @Resource
    private BeanFactory beanFactory;

    public StateMachine<OrderStatusEnum, OrderStatusChangeEventEnum> build() throws Exception {
        StateMachineBuilder.Builder<OrderStatusEnum, OrderStatusChangeEventEnum> builder = StateMachineBuilder.builder();
        builder.configureConfiguration().withConfiguration().machineId(MACHINE_ID).beanFactory(beanFactory);

        builder.configureStates().withStates().initial(OrderStatusEnum.CREATED).states(EnumSet.allOf(OrderStatusEnum.class));

        builder.configureTransitions()
                // 客户完成工单创建
                .withExternal().source(OrderStatusEnum.CREATED).target(OrderStatusEnum.WAITING).event(OrderStatusChangeEventEnum.CLIENT_COMPLETED_ORDER_CREATED).action(action())
                // 工程师受理工单
                .and().withExternal().source(OrderStatusEnum.WAITING).target(OrderStatusEnum.ACCEPTED).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_ACCEPT)
                // 工程师开始检查
                .and().withExternal().source(OrderStatusEnum.ACCEPTED).target(OrderStatusEnum.CHECKING).event(OrderStatusChangeEventEnum.ENGINEER_START_CHECKING)
                // 工程师确认计费
                .and().withExternal().source(OrderStatusEnum.CHECKING).target(OrderStatusEnum.FEE_CONFIRMING).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_FEE_CONFIRM)
                // 用户确认账单
                .and().withExternal().source(OrderStatusEnum.FEE_CONFIRMING).target(OrderStatusEnum.FEE_CONFIRMED).event(OrderStatusChangeEventEnum.CLIENT_COMPLETED_FEE_CONFIRM)
                // 工程师申请物料
                .and().withExternal().source(OrderStatusEnum.FEE_CONFIRMED).target(OrderStatusEnum.MATERIAL_APPLYING).event(OrderStatusChangeEventEnum.ENGINEER_APPLIED_MATERIAL)
                // 库管分发物料
                .and().withExternal().source(OrderStatusEnum.MATERIAL_APPLYING).target(OrderStatusEnum.MATERIAL_DISTRIBUTING).event(OrderStatusChangeEventEnum.MANAGER_DISTRIBUTED_MATERIAL)
                // 工程师收到物料，开始维修
                .and().withExternal().source(OrderStatusEnum.MATERIAL_DISTRIBUTING).target(OrderStatusEnum.REPAIRING).event(OrderStatusChangeEventEnum.ENGINEER_RECEIVED_MATERIAL)
                // 工程师开始复检
                .and().withExternal().source(OrderStatusEnum.REPAIRING).target(OrderStatusEnum.RE_CHECKING).event(OrderStatusChangeEventEnum.ENGINEER_START_RECHECK)
                // 工程师维修结束，等待客户支付
                .and().withExternal().source(OrderStatusEnum.RE_CHECKING).target(OrderStatusEnum.TO_BE_PAID).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RECHECK)
                // 客户完成支付
                .and().withExternal().source(OrderStatusEnum.TO_BE_PAID).target(OrderStatusEnum.PAID).event(OrderStatusChangeEventEnum.CLIENT_COMPLETED_PAY)
                // 工程师返还物件
                .and().withExternal().source(OrderStatusEnum.PAID).target(OrderStatusEnum.RETURNING).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RETURN)
                // 客户关闭工单
                .and().withExternal().source(OrderStatusEnum.RETURNING).target(OrderStatusEnum.CLOSED).event(OrderStatusChangeEventEnum.CLIENT_CLOSED)
                // 无需申请物料，直接开始维修
                .and().withExternal().source(OrderStatusEnum.FEE_CONFIRMED).target(OrderStatusEnum.REPAIRING).event(OrderStatusChangeEventEnum.ENGINEER_START_REPAIR);

        return builder.build();
    }

    @Bean
    public Action<OrderStatusEnum, OrderStatusChangeEventEnum> action() {
        return new Action<>() {
            @Override
            public void execute(StateContext<OrderStatusEnum, OrderStatusChangeEventEnum> context) {
                System.out.println(context);
            }
        };
    }
//
//    /**
//     * 配置状态
//     */
//    @Override
//    public void configure(StateMachineStateConfigurer<OrderStatusEnum, OrderStatusChangeEventEnum> states) throws Exception {
//        states.withStates().initial(OrderStatusEnum.CREATED).end(OrderStatusEnum.CLOSED).states(EnumSet.allOf(OrderStatusEnum.class));
//    }
//
//    /**
//     * 配置状态转换事件关系
//     */
//    @Override
//    public void configure(StateMachineTransitionConfigurer<OrderStatusEnum, OrderStatusChangeEventEnum> transitions) throws Exception {
//        transitions.withExternal().source(OrderStatusEnum.CREATED).target(OrderStatusEnum.WAITING).event(OrderStatusChangeEventEnum.CLIENT_COMPLETED_ORDER_CREATED)
//
//                .and().withExternal().source(OrderStatusEnum.WAITING).target(OrderStatusEnum.ACCEPTED).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_ACCEPT)
//
//                .and().withExternal().source(OrderStatusEnum.ACCEPTED).target(OrderStatusEnum.CHECKING).event(OrderStatusChangeEventEnum.ENGINEER_START_CHECKING)
//
//                .and().withExternal().source(OrderStatusEnum.CHECKING).target(OrderStatusEnum.FEE_CONFIRMING).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_FEE_CONFIRM)
//
//                .and().withExternal().source(OrderStatusEnum.FEE_CONFIRMING).target(OrderStatusEnum.MATERIAL_APPLYING).event(OrderStatusChangeEventEnum.ENGINEER_APPLIED_MATERIAL)
//
//                .and().withExternal().source(OrderStatusEnum.MATERIAL_APPLYING).target(OrderStatusEnum.MATERIAL_DISTRIBUTING).event(OrderStatusChangeEventEnum.MANAGER_DISTRIBUTED_MATERIAL)
//
//                .and().withExternal().source(OrderStatusEnum.MATERIAL_DISTRIBUTING).target(OrderStatusEnum.REPAIRING).event(OrderStatusChangeEventEnum.ENGINEER_RECEIVED_MATERIAL)
//
//                .and().withExternal().source(OrderStatusEnum.REPAIRING).target(OrderStatusEnum.RE_CHECKING).event(OrderStatusChangeEventEnum.ENGINEER_START_RECHECK)
//
//                .and().withExternal().source(OrderStatusEnum.RE_CHECKING).target(OrderStatusEnum.TO_BE_PAID).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RECHECK)
//
//                .and().withExternal().source(OrderStatusEnum.TO_BE_PAID).target(OrderStatusEnum.PAID).event(OrderStatusChangeEventEnum.CLIENT_COMPLETED_PAY)
//
//                .and().withExternal().source(OrderStatusEnum.PAID).target(OrderStatusEnum.RETURNING).event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RETURN)
//
//                .and().withExternal().source(OrderStatusEnum.RETURNING).target(OrderStatusEnum.CLOSED).event(OrderStatusChangeEventEnum.CLIENT_CLOSED)
//
//                .and().withExternal().source(OrderStatusEnum.FEE_CONFIRMING).target(OrderStatusEnum.REPAIRING).event(OrderStatusChangeEventEnum.ENGINEER_START_REPAIR);
//    }
}