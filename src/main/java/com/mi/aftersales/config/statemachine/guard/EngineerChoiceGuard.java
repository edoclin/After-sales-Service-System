package com.mi.aftersales.config.statemachine.guard;

import com.mi.aftersales.config.enums.OrderStatusChangeEventEnum;
import com.mi.aftersales.entity.enums.OrderStatusEnum;
import com.mi.aftersales.service.OrderService;
import org.springframework.statemachine.StateContext;
import org.springframework.statemachine.guard.Guard;


public class EngineerChoiceGuard implements Guard<OrderStatusEnum, OrderStatusChangeEventEnum> {

    @Override
    public boolean evaluate(StateContext<OrderStatusEnum, OrderStatusChangeEventEnum> stateContext) {
        OrderStatusChangeEventEnum choice = (OrderStatusChangeEventEnum) stateContext.getMessageHeader(OrderService.ENGINEER_CHOICE);
        boolean result = choice == OrderStatusChangeEventEnum.ENGINEER_START_REPAIR;
        return result;
    }
}
