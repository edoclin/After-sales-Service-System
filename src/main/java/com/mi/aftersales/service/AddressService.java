package com.mi.aftersales.service;

import com.mi.aftersales.vo.form.ClientAddressForm;
import com.mi.aftersales.vo.result.ClientAddressVo;

import java.util.List;

/**
 * <p>
 * 客户联系地址 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface AddressService{
    /**
     * 添加客户地址。
     *
     * <p>根据提供的地址表单和登录 ID 添加新的地址记录。如果新地址设置为默认地址，
     * 则会将当前用户的其他地址设置为非默认地址。</p>
     *
     * @param form 地址表单，包含地址信息。
     * @param loginId 当前登录用户的 ID。
     */
    void addAddress(ClientAddressForm form, String loginId);

    /**
     * 获取指定用户的地址列表。
     *
     * <p>根据用户的登录 ID 查询该用户的所有地址信息，并进行适当的处理和格式化。</p>
     *
     * @param loginId 用户的登录 ID。
     * @return 包含用户地址信息的列表，每个地址信息为 {@link ClientAddressVo} 对象。
     */

    List<ClientAddressVo> listAddress(String loginId);

    /**
     * 设置用户的默认地址。
     *
     * <p>根据地址 ID 和登录 ID，将该地址设置为默认地址，并将用户的其他地址设置为非默认地址。</p>
     *
     * @param addressId 地址 ID。
     * @param loginId 当前登录用户的 ID。
     */
    void setDefaultAddress(String addressId, String loginId);

    /**
     * 删除用户的指定地址。
     *
     * <p>根据地址 ID 和登录 ID 删除指定的地址记录。如果地址不存在或者地址不属于当前用户，
     * 则会抛出相应的异常。</p>
     *
     * @param addressId 地址 ID。
     * @param loginId 当前登录用户的 ID。
     */
    void removeAddress(String addressId, String loginId);
}
