package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.DesensitizedUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Address;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.mapper.AddressMapper;
import com.mi.aftersales.service.IAddressService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.mi.aftersales.vo.form.ClientAddressForm;
import com.mi.aftersales.vo.result.ClientAddressVo;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 客户联系地址 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class AddressServiceImpl extends ServiceImpl<AddressMapper, Address> implements IAddressService {

    @Resource
    private AddressMapper addressMapper;
    @Override
    public void addAddress(ClientAddressForm form, String loginId) {
        try {
            if (Boolean.TRUE.equals(form.getDefaulted())) {
                LambdaUpdateWrapper<Address> updateWrapper = new LambdaUpdateWrapper<>();
                updateWrapper.eq(Address::getLoginId, loginId).set(Address::getDefaulted, Boolean.FALSE);
                addressMapper.update(null, updateWrapper);
            }
            Address address = new Address();
            BeanUtil.copyProperties(form, address);
            address.setLoginId(loginId);
            addressMapper.insert(address);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @Override
    public List<ClientAddressVo> listAddress(String loginId) {
        LambdaQueryWrapper<Address> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Address::getLoginId, loginId);
        List<Address> addressList = addressMapper.selectList(queryWrapper);
        List<ClientAddressVo> result = new ArrayList<>();
        for (Address address : addressList) {
            ClientAddressVo item = new ClientAddressVo();
            BeanUtil.copyProperties(address, item);
            item.setAddressDetail(CharSequenceUtil.format("{}（{}）", address.getAddressDetail(), address.getRegion()));
            item.setMobile(DesensitizedUtil.mobilePhone(item.getMobile()));
            result.add(item);
        }
        return result;
    }

    @Override
    public void setDefaultAddress(String addressId, String loginId) {
        try {
            LambdaUpdateWrapper<Address> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.eq(Address::getLoginId, loginId).set(Address::getDefaulted, Boolean.FALSE);
            addressMapper.update(null, updateWrapper);

            LambdaUpdateWrapper<Address> updateWrapper2 = new LambdaUpdateWrapper<>();
            updateWrapper2.eq(Address::getAddressId, addressId).eq(Address::getLoginId, loginId).set(Address::getDefaulted, Boolean.TRUE);
            addressMapper.update(null, updateWrapper2);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @Override
    public void removeAddress(String addressId, String loginId) {
        try {
            Address address = addressMapper.selectById(addressId);
            if (BeanUtil.isEmpty(address)) {
                throw new GracefulResponseException("地址不存在！");
            }
            if (!CharSequenceUtil.equals(address.getLoginId(), loginId)) {
                throw new GracefulResponseException("非法操作！");
            }
            addressMapper.deleteById(addressId);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

}
