package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.EmployeeInfo;
import com.mi.aftersales.mapper.EmployeeInfoMapper;
import com.mi.aftersales.service.iservice.IEmployeeInfoService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 员工信息表 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IEmployeeInfoServiceImpl extends ServiceImpl<EmployeeInfoMapper, EmployeeInfo> implements IEmployeeInfoService {

}
