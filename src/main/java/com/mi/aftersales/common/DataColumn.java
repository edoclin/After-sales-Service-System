package com.mi.aftersales.common;


import lombok.Data;

/**
 * @description: 前端查询表格header
 * @return:
 * @author: edoclin
 * @created: 2024/6/3 17:20
 **/
@Data
public class DataColumn {
    private String field;
    private String label;
    private Boolean sortable;
    private Boolean fixed;
    private Integer index;
}
