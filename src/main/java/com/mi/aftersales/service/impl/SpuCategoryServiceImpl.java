package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.SpuCategory;
import com.mi.aftersales.exception.graceful.IllegalSpuCategoryIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.pojo.vo.SpuCategory4ClientVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryFormVo;
import com.mi.aftersales.pojo.vo.form.SpuCategoryVisibleSetFormVo;
import com.mi.aftersales.pojo.vo.form.UpdateSpuCategoryFormVo;
import com.mi.aftersales.repository.ISpuCategoryRepository;
import com.mi.aftersales.service.SpuCategoryService;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.mi.aftersales.common.RedisNamespace.SPU_CATEGORY_CACHE_LOCK_PREFIX;
import static com.mi.aftersales.common.RedisNamespace.SPU_CATEGORY_CACHE_PREFIX;

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

    @Resource
    private RedisTemplate<String, SpuCategory4ClientVo> redisTemplate;

    @Resource
    private RedissonClient redissonClient;

    @Override
    public void addSpuCategory(SpuCategoryFormVo form) {
        if (form.getParentCategoryId() != 0 && BeanUtil.isEmpty(iSpuCategoryRepository.getById(form.getParentCategoryId()))) {
            throw new IllegalSpuCategoryIdException();
        }
        SpuCategory spuCategory = new SpuCategory();
        BeanUtil.copyProperties(form, spuCategory);

        iSpuCategoryRepository.save(spuCategory);

    }

    @Override
    public void updateSpuCategory(UpdateSpuCategoryFormVo form) {
        if (form.getParentCategoryId() != 0 && BeanUtil.isEmpty(iSpuCategoryRepository.getById(form.getParentCategoryId()))) {
            throw new IllegalSpuCategoryIdException();
        }
        SpuCategory spuCategory = new SpuCategory();
        BeanUtil.copyProperties(form, spuCategory);

        iSpuCategoryRepository.save(spuCategory);
    }

    @Override
    public void deleteSpuCategoryById(Integer categoryId) {
        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(categoryId))) {
            throw new IllegalSpuCategoryIdException();
        }

        if (CollUtil.isNotEmpty(iSpuCategoryRepository.lambdaQuery().eq(SpuCategory::getParentCategoryId, categoryId).list())) {
            throw new GracefulResponseException("存在依赖于当前分类的子分类！");
        }


        if (Boolean.FALSE.equals(iSpuCategoryRepository.removeById(categoryId))) {
            throw new ServerErrorException();
        }
    }

    @Override
    public List<Integer> childrenCategoryId(Integer parentCategoryId) {
        List<Integer> result = new ArrayList<>();
        if (parentCategoryId != null) {
            result.add(parentCategoryId);
            iSpuCategoryRepository
                    .lambdaQuery()
                    .eq(SpuCategory::getParentCategoryId, parentCategoryId)
                    .list().forEach(item -> result.addAll(childrenCategoryId(item.getCategoryId())));
        }
        return result;
    }


    @Override
    public void setSpuCategoryVisibility(SpuCategoryVisibleSetFormVo form) {
        SpuCategory spuCategory = iSpuCategoryRepository.getById(form.getCategoryId());
        if (BeanUtil.isEmpty(spuCategory)) {
            throw new IllegalSpuCategoryIdException();
        }

        spuCategory.setVisible(form.getVisible());
        if (!iSpuCategoryRepository.updateById(spuCategory)) {
            throw new ServerErrorException();
        }
    }

    @Override
    public List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId) {
        String key = SPU_CATEGORY_CACHE_PREFIX + parentId;
        // 双重校验
        if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
            RLock fairLock = redissonClient.getFairLock(SPU_CATEGORY_CACHE_LOCK_PREFIX + parentId);
            if (fairLock == null) {
                throw new ServerErrorException();
            }
            if (fairLock.tryLock()) {
                if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
                    List<SpuCategory4ClientVo> result = iSpuCategoryRepository.listSpuCategory4Client(parentId);
                    redisTemplate.opsForList().leftPushAll(key, result);
                    redisTemplate.expire(key, 60, TimeUnit.SECONDS);
                }
                fairLock.unlock();
            }
        }
        return redisTemplate.opsForList().range(key, 0, -1);
    }

}
