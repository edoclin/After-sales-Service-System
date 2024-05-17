package com.mi.aftersales.config;

import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import org.springframework.context.annotation.Configuration;
import org.springframework.statemachine.config.EnableStateMachine;
import org.springframework.statemachine.config.StateMachineConfigurerAdapter;
import org.springframework.statemachine.config.builders.StateMachineStateConfigurer;
import org.springframework.statemachine.config.builders.StateMachineTransitionConfigurer;

import java.util.EnumSet;

/**
 * @description: 工单状态机
 * @return:
 * @author: edoclin
 * @created: 2024/5/16 15:19
 **/
@Configuration
@EnableStateMachine
public class OrderStatusMachineConfig extends StateMachineConfigurerAdapter<OrderStatusEnum, OrderStatusChangeEventEnum> {

    /**
     * 配置状态
     */
    @Override
    public void configure(StateMachineStateConfigurer<OrderStatusEnum, OrderStatusChangeEventEnum> states) throws Exception {
        states.withStates().initial(OrderStatusEnum.CREATED).end(OrderStatusEnum.CLOSED).states(EnumSet.allOf(OrderStatusEnum.class));
    }

    /**
     * 配置状态转换事件关系
     */
    @Override
    public void configure(StateMachineTransitionConfigurer<OrderStatusEnum, OrderStatusChangeEventEnum> transitions) throws Exception {
        transitions
                .withExternal().source(OrderStatusEnum.WAITING).target(OrderStatusEnum.ACCEPTED)
                .event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_ACCEPT)

                .and().withExternal().source(OrderStatusEnum.ACCEPTED).target(OrderStatusEnum.CHECKING)
                .event(OrderStatusChangeEventEnum.ENGINEER_START_CHECKING)

                .and().withExternal().source(OrderStatusEnum.CHECKING).target(OrderStatusEnum.FEE_CONFIRMING)
                .event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_FEE_CONFIRM)

                .and().withExternal().source(OrderStatusEnum.FEE_CONFIRMING).target(OrderStatusEnum.MATERIAL_APPLYING)
                .event(OrderStatusChangeEventEnum.ENGINEER_APPLIED_MATERIAL)

                .and().withExternal().source(OrderStatusEnum.MATERIAL_APPLYING).target(OrderStatusEnum.MATERIAL_DISTRIBUTING)
                .event(OrderStatusChangeEventEnum.MANAGER_DISTRIBUTED_MATERIAL)

                .and().withExternal().source(OrderStatusEnum.MATERIAL_DISTRIBUTING).target(OrderStatusEnum.REPAIRING)
                .event(OrderStatusChangeEventEnum.ENGINEER_RECEIVED_MATERIAL)

                .and().withExternal().source(OrderStatusEnum.REPAIRING).target(OrderStatusEnum.RE_CHECKING)
                .event(OrderStatusChangeEventEnum.ENGINEER_START_RECHECK)

                .and().withExternal().source(OrderStatusEnum.RE_CHECKING).target(OrderStatusEnum.TO_BE_PAID)
                .event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RECHECK)

                .and().withExternal().source(OrderStatusEnum.TO_BE_PAID).target(OrderStatusEnum.PAID)
                .event(OrderStatusChangeEventEnum.CLIENT_COMPLETED_PAY)

                .and().withExternal().source(OrderStatusEnum.PAID).target(OrderStatusEnum.RETURNING)
                .event(OrderStatusChangeEventEnum.ENGINEER_COMPLETED_RETURN)

                .and().withExternal().source(OrderStatusEnum.RETURNING).target(OrderStatusEnum.CLOSED)
                .event(OrderStatusChangeEventEnum.CLIENT_CLOSED)

                .and().withExternal().source(OrderStatusEnum.FEE_CONFIRMING).target(OrderStatusEnum.REPAIRING)
                .event(OrderStatusChangeEventEnum.ENGINEER_START_REPAIR)
        ;
    }
}