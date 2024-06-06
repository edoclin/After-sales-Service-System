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

    public static final String ORDER_LOCK_PREFIX = "lock:order:";
    public static final String MATERIAL_LOCK_PREFIX = "lock:material:";
    public static final String SPU_CATEGORY_CACHE_LOCK_PREFIX = "lock:spu:category:";

    public static final String PENDING_ORDER_PREFIX = "order:pending:";
    public static final String MACHINE_PERSIST_PREFIX = "machine:persist:";
    public static final String SPU_CATEGORY_CACHE_PREFIX = "spu:category:";
}
