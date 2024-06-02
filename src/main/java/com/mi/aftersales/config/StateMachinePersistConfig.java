package com.mi.aftersales.config;

import com.mi.aftersales.enums.config.OrderStatusChangeEventEnum;
import com.mi.aftersales.enums.entity.OrderStatusEnum;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.statemachine.StateMachinePersist;
import org.springframework.statemachine.data.redis.RedisStateMachineContextRepository;
import org.springframework.statemachine.data.redis.RedisStateMachinePersister;
import org.springframework.statemachine.persist.RepositoryStateMachinePersist;

import javax.annotation.Resource;

@Configuration
public class StateMachinePersistConfig {

    @Resource
    private RedisConnectionFactory redisConnectionFactory;


    /**
     * 注入RedisStateMachinePersister对象
     */
    @Bean(name = "orderRedisPersister")
    public RedisStateMachinePersister<OrderStatusEnum, OrderStatusChangeEventEnum> redisPersister() {
        return new RedisStateMachinePersister<>(redisPersist());
    }

    /**
     * 通过redisConnectionFactory创建StateMachinePersist
     */
    public StateMachinePersist<OrderStatusEnum, OrderStatusChangeEventEnum, String> redisPersist() {
        RedisStateMachineContextRepository<OrderStatusEnum, OrderStatusChangeEventEnum> repository = new RedisStateMachineContextRepository<>(redisConnectionFactory);
        return new RepositoryStateMachinePersist<>(repository);
    }
}