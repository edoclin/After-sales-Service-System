package com.mi.aftersales.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.text.CharSequenceUtil;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.Fapiao;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.FapiaoService;
import com.mi.aftersales.service.iservice.IFapiaoService;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.vo.form.FapiaoForm;
import com.mi.aftersales.vo.form.UpdateFapiaoForm;
import com.mi.aftersales.vo.result.ClientFapiaoVo;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 发票信息 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class FapiaoServiceImpl implements FapiaoService {
    @Resource
    private IFapiaoService iFapiaoService;

    @Override
    public void addFapiao(FapiaoForm form) {
        Fapiao fapiao = new Fapiao();
        BeanUtil.copyProperties(form, fapiao);
        try {
            iFapiaoService.save(fapiao);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("发票号码已存在！");
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @Override
    public List<ClientFapiaoVo> listFapiaoByClient(String loginId) {
        ArrayList<ClientFapiaoVo> result = new ArrayList<>();
        iFapiaoService.lambdaQuery().eq(Fapiao::getCreatedId, loginId).list().forEach(fapiao -> {
            ClientFapiaoVo item = new ClientFapiaoVo();
            BeanUtil.copyProperties(fapiao, item, DateUtil.copyDate2yyyyMMddHHmm());
            result.add(item);
        });
        return result;
    }

    @Override
    public void deleteFapiaoByClient(String fapiaoId, String loginId) {
        Fapiao byId = iFapiaoService.getById(fapiaoId);
        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("发票ID不存在！");
        }
        if (!CharSequenceUtil.equals(byId.getCreatedId(), loginId)) {
            throw new GracefulResponseException("该发票不属于当前用户！");
        }
        try {
            iFapiaoService.removeById(fapiaoId);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }

    @Override
    public void updateFapiaoByClient(UpdateFapiaoForm form, String loginId) {
        Fapiao byId = iFapiaoService.getById(form.getFapiaoId());
        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("发票ID不存在！");
        }
        if (!CharSequenceUtil.equals(byId.getCreatedId(), loginId)) {
            throw new GracefulResponseException("该发票不属于当前用户！");
        }
        BeanUtil.copyProperties(form, byId, CopyOptions.create().ignoreNullValue());
        try {
            iFapiaoService.updateById(byId);
        } catch (Exception e) {
            throw new ServerErrorException(e.getMessage());
        }
    }
}
