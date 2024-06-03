package com.mi.aftersales.statemachine.guard;

import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.enums.entity.OrderStatusEnum;
import com.mi.aftersales.service.OrderService;
import org.springframework.statemachine.StateContext;
import org.springframework.statemachine.guard.Guard;

public class ClientChoiceGuard implements Guard<OrderStatusEnum, OrderStatusChangeEventEnum> {

    @Override
    public boolean evaluate(StateContext<OrderStatusEnum, OrderStatusChangeEventEnum> stateContext) {
        OrderStatusChangeEventEnum choice = (OrderStatusChangeEventEnum) stateContext.getMessageHeader(OrderService.CLIENT_CHOICE);
        boolean result = choice == OrderStatusChangeEventEnum.CLIENT_REJECT_REPAIR;
        return result;
    }
}
