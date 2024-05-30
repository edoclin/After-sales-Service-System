package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.OrderUpload;
import com.mi.aftersales.mapper.OrderUploadMapper;
import com.mi.aftersales.repository.IOrderUploadRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 工单的附属文件 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IOrderUploadRepositoryImpl extends ServiceImpl<OrderUploadMapper, OrderUpload> implements IOrderUploadRepository {

}
