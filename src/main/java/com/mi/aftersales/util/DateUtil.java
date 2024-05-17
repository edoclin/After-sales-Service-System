package com.mi.aftersales.util;

import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.util.ObjectUtil;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.Set;

/**
 * @description: 日期格式化工具
 * @return:
 * @author: edoclin
 * @created: 2024/5/17 18:22
 **/
public class DateUtil {

    private static final Set<String> COVERTED_SET = new HashSet<>();

    static {
        COVERTED_SET.add("createdTime");
        COVERTED_SET.add("updatedTime");
        COVERTED_SET.add("fapiaoTime");
        COVERTED_SET.add("releasedTime");
    }


    public static String yyyyMMddHHmm(LocalDateTime dateTime) {
        return ObjectUtil.isNull(dateTime) ? null : dateTime.format(DateTimeFormatter.ofPattern("yyyy年MM月dd日 HH时mm分"));
    }

    public static CopyOptions copyDate2yyyyMMddHHmm() {
        return CopyOptions.create().setFieldValueEditor((name, value) -> {
            if (value != null && COVERTED_SET.contains(name)) {
                return yyyyMMddHHmm(LocalDateTimeUtil.parse((CharSequence) value));
            } else {
                return value;
            }

        });
    }
}
