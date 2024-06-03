package com.mi.aftersales.util.view;

import cn.hutool.core.annotation.AnnotationUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ReflectUtil;
import com.mi.aftersales.pojo.common.DataColumn;
import com.mi.aftersales.util.view.anno.View;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * @description: 前端查询工具类
 * @return:
 * @author: edoclin
 * @created: 2024/6/3 21:38
 **/
public class ViewUtil {

    private ViewUtil() {}
    public static List<DataColumn> dataColumns(Class voClass) {
        ArrayList<DataColumn> result = new ArrayList<>();

        for (Field field : ReflectUtil.getFields(voClass)) {

            View view = AnnotationUtil.getAnnotation(field, View.class);
            if (view != null) {
                DataColumn dataColumn = new DataColumn();
                dataColumn.setField(field.getName());
                dataColumn.setLabel(view.label());
                dataColumn.setSortable(view.sortable());
                dataColumn.setFixed(view.fixed());
                dataColumn.setIndex(view.index());

                result.add(dataColumn);
            }
        }
        CollUtil.sort(result, Comparator.comparingInt(DataColumn::getIndex));
        return result;
    }

}
