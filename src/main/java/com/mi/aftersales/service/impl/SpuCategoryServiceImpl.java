package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.SpuCategory;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.SpuCategoryService;
import com.mi.aftersales.repository.ISpuCategoryRepository;
import com.mi.aftersales.vo.form.SpuCategoryForm;
import com.mi.aftersales.vo.form.SpuCategoryVisibleSetForm;
import com.mi.aftersales.vo.result.SpuCategory4ClientVo;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * <p>
 * 商品分类 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class SpuCategoryServiceImpl implements SpuCategoryService {
    @Resource
    private ISpuCategoryRepository iSpuCategoryRepository;

    @Override
    public void addSpuCategory(SpuCategoryForm form) {
        if (form.getParentCategoryId() != 0 && BeanUtil.isEmpty(iSpuCategoryRepository.getById(form.getParentCategoryId()))) {
            throw new GracefulResponseException("指定父级分类不存在");
        }
        SpuCategory spuCategory = new SpuCategory();
        BeanUtil.copyProperties(form, spuCategory);

        try {
            iSpuCategoryRepository.save(spuCategory);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("SPU分类名称已存在");
        }
    }


    @Override
    public void setSpuCategoryVisibility(SpuCategoryVisibleSetForm form) {
        SpuCategory spuCategory = iSpuCategoryRepository.getById(form.getCategoryId());
        if (BeanUtil.isEmpty(spuCategory)) {
            throw new GracefulResponseException("分类ID不存在");
        }

        spuCategory.setVisible(form.getVisible());
        if (!iSpuCategoryRepository.updateById(spuCategory)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId) {
        return iSpuCategoryRepository.listSpuCategory4Client(parentId);
    }

}
