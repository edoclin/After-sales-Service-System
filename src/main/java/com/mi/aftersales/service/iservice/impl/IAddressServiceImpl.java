package com.mi.aftersales.service.iservice.impl;

import com.mi.aftersales.entity.Address;
import com.mi.aftersales.mapper.AddressMapper;
import com.mi.aftersales.service.iservice.IAddressService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;


/**
 * <p>
 * 客户联系地址 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class IAddressServiceImpl extends ServiceImpl<AddressMapper, Address> implements IAddressService {

}
