package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.DesensitizedUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.Address;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.IAddressService;
import com.mi.aftersales.vo.form.ClientAddressForm;
import com.mi.aftersales.vo.result.ClientAddressVo;
import io.swagger.v3.oas.annotations.Operation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 客户联系地址 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/address")
public class AddressController {

    private static final Logger log = LoggerFactory.getLogger(AddressController.class);


    @Resource
    private IAddressService iAddressService;

    @PostMapping(path = "/")
    @Operation(summary = "客户添加地址", description = "客户添加地址")
    @CheckLogin
    public void postAddress(@RequestBody @Valid ClientAddressForm form) {
        if (Boolean.TRUE.equals(form.getDefaulted())) {
            iAddressService.lambdaUpdate().eq(Address::getLoginId, StpUtil.getLoginIdAsString()).set(Address::getDefaulted, Boolean.FALSE).update();
        }
        Address address = new Address();
        BeanUtil.copyProperties(form, address);
        address.setLoginId(StpUtil.getLoginIdAsString());
        try {
            iAddressService.save(address);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @GetMapping(path = "/")
    @Operation(summary = "客户查询地址", description = "客户查询地址")
    @CheckLogin
    public List<ClientAddressVo> listAddress() {
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

    @PutMapping(path = "/defaulted/{addressId}")
    @Operation(summary = "客户设置默认地址", description = "客户设置默认地址")
    @CheckLogin
    public void defaultAddress(@PathVariable String addressId) {
        try {
            iAddressService.lambdaUpdate().eq(Address::getLoginId, StpUtil.getLoginIdAsString()).set(Address::getDefaulted, Boolean.FALSE).update();
            iAddressService.lambdaUpdate().eq(Address::getAddressId, addressId).eq(Address::getLoginId, StpUtil.getLoginIdAsString()).set(Address::getDefaulted, Boolean.TRUE).update();
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }

    }

    @DeleteMapping(path = "/{addressId}")
    @Operation(summary = "客户删除地址", description = "客户删除地址")
    @CheckLogin
    public void deleteAddress(@PathVariable String addressId) {
        Address byId = iAddressService.getById(addressId);
        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("地址不存在！");
        }
        if (!CharSequenceUtil.equals(byId.getLoginId(), StpUtil.getLoginIdAsString())) {
            throw new GracefulResponseException("非法操作！");
        }
        try {
            iAddressService.removeById(addressId);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }
}
