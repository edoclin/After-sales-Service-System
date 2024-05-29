package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.Rating;
import com.mi.aftersales.mapper.RatingMapper;
import com.mi.aftersales.service.iservice.IRatingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 工单评价 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IRatingServiceImpl extends ServiceImpl<RatingMapper, Rating> implements IRatingService {

}
