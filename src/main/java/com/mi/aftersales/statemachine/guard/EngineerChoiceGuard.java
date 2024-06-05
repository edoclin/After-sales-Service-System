package com.mi.aftersales.statemachine.guard;

import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.enums.entity.OrderStatusEnum;
import org.springframework.statemachine.StateContext;
import org.springframework.statemachine.guard.Guard;

import static com.mi.aftersales.common.StateMachineOrderStatus.ENGINEER_CHOICE;

/**
 * @description: 工程师维修状态分支
 * @return:
 * @author: edoclin
 * @created: 2024/6/5 21:33
 **/
public class EngineerChoiceGuard implements Guard<OrderStatusEnum, OrderStatusChangeEventEnum> {

    @Override
    public boolean evaluate(StateContext<OrderStatusEnum, OrderStatusChangeEventEnum> stateContext) {
        OrderStatusChangeEventEnum choice = (OrderStatusChangeEventEnum) stateContext.getMessageHeader(ENGINEER_CHOICE);
        return choice == OrderStatusChangeEventEnum.ENGINEER_START_REPAIR;
    }
}
