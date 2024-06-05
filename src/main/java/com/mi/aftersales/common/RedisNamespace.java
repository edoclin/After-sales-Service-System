package com.mi.aftersales.common;


/**
 * @description: 分布式锁
 * @return:
 * @author: edoclin
 * @created: 2024/6/5 21:24
 **/
public class RedisNamespace {
    private RedisNamespace() {
    }

    public static final String ORDER_LOCK_PREFIX = "order:lock:";
    public static final String MATERIAL_LOCK_PREFIX = "material:lock:";

    public static final String PENDING_ORDER_PREFIX = "order:pending:";
    public static final String MACHINE_PERSIST_PREFIX = "machine:persist:";
}
