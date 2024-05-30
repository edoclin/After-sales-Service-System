package com.mi.aftersales.entity.enums;

import com.baomidou.mybatisplus.annotation.IEnum;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/14 13:44
 **/
public enum OrderUploadFileTypeEnum implements IEnum<Integer> {
    // 文件类型
    IMAGE(1, "图片"),
    VIDEO(2, "视频"),
    ;
    private int value;
    private String desc;

    OrderUploadFileTypeEnum(int value, String desc) {
        this.value = value;
        this.desc = desc;
    }

    @Override
    public Integer getValue() {
        return value;
    }
}
