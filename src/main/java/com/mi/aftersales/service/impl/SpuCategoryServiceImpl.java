package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.SpuCategory;
import com.mi.aftersales.mapper.SpuCategoryMapper;
import com.mi.aftersales.service.ISpuCategoryService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.vo.result.SpuCategory4ClientVo;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

/**
 * <p>
 * 商品分类 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SpuCategoryServiceImpl extends ServiceImpl<SpuCategoryMapper, SpuCategory> implements ISpuCategoryService {

    @Resource
    private SpuCategoryMapper spuCategoryMapper;

    @Override
    public List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId) {
        return spuCategoryMapper.listSpuCategory4Client(parentId);
    }

    @Override
    public List<String> listAllSpuCategoryName(Integer categoryId) {
        Stack<String> stack = new Stack<>();
        List<String> result = new ArrayList<>();
        SpuCategory spuCategory;
        while (categoryId != 0) {
            spuCategory = spuCategoryMapper.selectById(categoryId);
            if (BeanUtil.isEmpty(spuCategory)) {
                throw new GracefulResponseException("非法的分类Id");
            }
            stack.push(spuCategory.getCategoryName());
            categoryId = spuCategory.getParentCategoryId();
        }

        while (!stack.isEmpty()) {
            result.add(stack.pop());
        }
        return result;
    }
}
