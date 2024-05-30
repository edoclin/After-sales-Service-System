package com.mi.aftersales.repository.impl;

import com.mi.aftersales.entity.File;
import com.mi.aftersales.mapper.FileMapper;
import com.mi.aftersales.repository.IFileRepository;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 对象存储 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IFileRepositoryImpl extends ServiceImpl<FileMapper, File> implements IFileRepository {

}
