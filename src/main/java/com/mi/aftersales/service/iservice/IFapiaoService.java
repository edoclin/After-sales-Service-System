package com.mi.aftersales.service.iservice;

import com.mi.aftersales.entity.Fapiao;
import com.baomidou.mybatisplus.extension.service.IService;
import com.mi.aftersales.vo.form.FapiaoForm;
import com.mi.aftersales.vo.form.UpdateFapiaoForm;
import com.mi.aftersales.vo.result.ClientFapiaoVo;

import java.util.List;

/**
 * <p>
 * 发票信息 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface IFapiaoService extends IService<Fapiao> {
    /**
     * 添加发票。
     *
     * @param form 发票表单
     */
    void addFapiao(FapiaoForm form);

    /**
     * 查询当前用户发票。
     *
     * @param loginId 用户登录ID
     * @return 发票列表
     */
    List<ClientFapiaoVo> listFapiaoByClient(String loginId);

    /**
     * 删除当前用户的发票。
     *
     * @param fapiaoId 发票ID
     * @param loginId 用户登录ID
     */
    void deleteFapiaoByClient(String fapiaoId, String loginId);

    /**
     * 更新当前用户的发票。
     *
     * @param form 更新发票表单
     * @param loginId 用户登录ID
     */
    void updateFapiaoByClient(UpdateFapiaoForm form, String loginId);
}
