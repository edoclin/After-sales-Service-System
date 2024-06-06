package com.mi.aftersales.statemachine.guard;

import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.enums.entity.OrderStatusEnum;
import com.mi.aftersales.service.OrderService;
import org.springframework.statemachine.StateContext;
import org.springframework.statemachine.guard.Guard;

import static com.mi.aftersales.common.StateMachineOrderStatus.CLIENT_CHOICE;

/**
 * @description: 客户确认维修分支
 * @return:
 * @author: edoclin
 * @created: 2024/6/5 21:34
 **/
public class ClientChoiceGuard implements Guard<OrderStatusEnum, OrderStatusChangeEventEnum> {

    @Override
    public boolean evaluate(StateContext<OrderStatusEnum, OrderStatusChangeEventEnum> stateContext) {
        OrderStatusChangeEventEnum choice = (OrderStatusChangeEventEnum) stateContext.getMessageHeader(CLIENT_CHOICE);
        return choice == OrderStatusChangeEventEnum.CLIENT_REJECT_REPAIR;
    }
}
