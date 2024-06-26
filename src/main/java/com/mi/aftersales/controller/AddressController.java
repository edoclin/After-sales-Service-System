package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.pojo.vo.ClientAddressVo;
import com.mi.aftersales.service.AddressService;
import com.mi.aftersales.pojo.vo.form.ClientAddressFormVo;
import io.swagger.v3.oas.annotations.Operation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
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
    private AddressService addressService;

    @PostMapping(path = "/client")
    @Operation(summary = "客户添加地址", description = "客户添加地址")
    @CheckLogin
    public void postAddress(@RequestBody @Valid ClientAddressFormVo form) {
        log.info("客户添加地址");
        addressService.addAddress(form, StpUtil.getLoginIdAsString());
    }

    @GetMapping(path = "/client")
    @Operation(summary = "客户查询所有地址", description = "客户查询所有地址")
    @CheckLogin
    public List<ClientAddressVo> listAddress() {
        return addressService.listAddress(StpUtil.getLoginIdAsString());
    }

    @PutMapping(path = "/client/defaulted/{addressId}")
    @Operation(summary = "客户设置默认地址", description = "客户设置默认地址")
    @CheckLogin
    public void defaultAddress(@PathVariable String addressId) {
        addressService.setDefaultAddress(addressId, StpUtil.getLoginIdAsString());
    }

    @DeleteMapping(path = "/client/{addressId}")
    @Operation(summary = "客户删除地址", description = "客户删除地址")
    @CheckLogin
    public void deleteAddress(@PathVariable String addressId) {
        addressService.removeAddress(addressId, StpUtil.getLoginIdAsString());
    }
}
