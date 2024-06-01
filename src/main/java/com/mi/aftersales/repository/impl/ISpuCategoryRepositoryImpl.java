package com.mi.aftersales.repository.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.SpuCategory;
import com.mi.aftersales.exception.graceful.IllegalSpuCategoryIdException;
import com.mi.aftersales.mapper.SpuCategoryMapper;
import com.mi.aftersales.repository.ISpuCategoryRepository;
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
public class ISpuCategoryRepositoryImpl extends ServiceImpl<SpuCategoryMapper, SpuCategory> implements ISpuCategoryRepository {
    @Resource
    private SpuCategoryMapper spuCategoryMapper;

    @Override
    public List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId) {
        return spuCategoryMapper.listSpuCategory4Client(parentId);
    }

    @Override
    public List<SpuCategory> listAllSpuCategoryName(Integer categoryId) {
        Stack<SpuCategory> stack = new Stack<>();
        List<SpuCategory> result = new ArrayList<>();
        SpuCategory spuCategory;
        while (categoryId != null && categoryId != 0) {
            spuCategory = spuCategoryMapper.selectById(categoryId);

            if (BeanUtil.isEmpty(spuCategory)) {
                throw new IllegalSpuCategoryIdException();
            }
            SpuCategory item = new SpuCategory();

            item.setCategoryId(spuCategory.getCategoryId());
            item.setCategoryName(spuCategory.getCategoryName());
            stack.push(item);
            categoryId = spuCategory.getParentCategoryId();
        }

        while (!stack.isEmpty()) {
            result.add(stack.pop());
        }
        return result;
    }
}
