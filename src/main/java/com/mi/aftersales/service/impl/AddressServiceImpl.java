package com.mi.aftersales.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.DesensitizedUtil;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Address;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.AddressService;
import com.mi.aftersales.service.iservice.IAddressService;
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
public class AddressServiceImpl implements AddressService {

    @Resource
    private IAddressService iAddressService;
    @Override
    public void addAddress(ClientAddressForm form, String loginId) {
        try {
            if (Boolean.TRUE.equals(form.getDefaulted())) {
                LambdaUpdateWrapper<Address> updateWrapper = new LambdaUpdateWrapper<>();
                updateWrapper.eq(Address::getLoginId, loginId).set(Address::getDefaulted, Boolean.FALSE);
                iAddressService.update(null, updateWrapper);
            }
            Address address = new Address();
            BeanUtil.copyProperties(form, address);
            address.setLoginId(loginId);
            iAddressService.save(address);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @Override
    public List<ClientAddressVo> listAddress(String loginId) {
        ArrayList<ClientAddressVo> result = new ArrayList<>();
        iAddressService.lambdaQuery().eq(Address::getLoginId, StpUtil.getLoginIdAsString()).list().forEach(address -> {
            ClientAddressVo item = new ClientAddressVo();
            BeanUtil.copyProperties(address, item);
            item.setAddressDetail(CharSequenceUtil.format("{}（{}）", address.getAddressDetail(), address.getRegion()));
            item.setMobile(DesensitizedUtil.mobilePhone(item.getMobile()));
            result.add(item);
        });
        return result;
    }

    @Override
    public void setDefaultAddress(String addressId, String loginId) {
        try {
            LambdaUpdateWrapper<Address> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.eq(Address::getLoginId, loginId).set(Address::getDefaulted, Boolean.FALSE);
            iAddressService.update(null, updateWrapper);

            LambdaUpdateWrapper<Address> updateWrapper2 = new LambdaUpdateWrapper<>();
            updateWrapper2.eq(Address::getAddressId, addressId).eq(Address::getLoginId, loginId).set(Address::getDefaulted, Boolean.TRUE);
            iAddressService.update(null, updateWrapper2);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @Override
    public void removeAddress(String addressId, String loginId) {
        try {
            Address address = iAddressService.getById(addressId);
            if (BeanUtil.isEmpty(address)) {
                throw new GracefulResponseException("地址不存在！");
            }
            if (!CharSequenceUtil.equals(address.getLoginId(), loginId)) {
                throw new GracefulResponseException("非法操作！");
            }
            iAddressService.removeById(addressId);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

}
