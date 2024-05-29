package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.MaterialLog;
import com.mi.aftersales.mapper.MaterialLogMapper;
import com.mi.aftersales.repository.IMaterialLogRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 物料日志 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IMaterialLogRepositoryImpl extends ServiceImpl<MaterialLogMapper, MaterialLog> implements IMaterialLogRepository {

}
