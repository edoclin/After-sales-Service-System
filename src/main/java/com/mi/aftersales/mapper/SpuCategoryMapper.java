package com.mi.aftersales.mapper;

import com.mi.aftersales.entity.SpuCategory;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.mi.aftersales.pojo.vo.SpuCategory4ClientVo;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * <p>
 * 商品分类 Mapper 接口
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Mapper
public interface SpuCategoryMapper extends BaseMapper<SpuCategory> {
    List<SpuCategory4ClientVo> listSpuCategory4Client(Integer parentId);
}
