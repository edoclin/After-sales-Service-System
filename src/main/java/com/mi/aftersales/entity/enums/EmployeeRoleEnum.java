package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum EmployeeRoleEnum implements IEnum<Integer> {
    // 员工类型
    ENGINEER(1, "工程师"),
    MATERIAL_MANAGER(2, "库管"),
    SYSTEM_MANAGER(3, "系统管理员"),

    ;
    private int value;
    private String desc;

    EmployeeRoleEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
