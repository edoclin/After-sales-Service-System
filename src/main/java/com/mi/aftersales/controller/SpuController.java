package com.mi.aftersales.controller;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Spu;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.service.iservice.IFileService;
import com.mi.aftersales.service.iservice.ISpuCategoryService;
import com.mi.aftersales.service.iservice.ISpuService;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.vo.result.ClientSpuVo;
import com.mi.aftersales.vo.PageResult;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.vo.result.SpuVo;
import com.mi.aftersales.vo.form.SpuForm;
import com.mi.aftersales.vo.form.UpdateSpuVisibleForm;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;

/**
 * <p>
 * 商品 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/spu")
public class SpuController {

    @Resource
    private ISpuService iSpuService;

    @Resource
    private ISpuCategoryService iSpuCategoryService;

    @Resource
    private IFileService iFileService;

    @PostMapping(path = "/")
    @Operation(summary = "管理员添加商品", description = "管理员添加商品")
    public void postSpu(@RequestBody @Valid SpuForm form) {

        if (BeanUtil.isEmpty(iSpuCategoryService.getById(form.getCategoryId()))) {
            throw new GracefulResponseException("商品所属分类不存在！");
        }

        if (BeanUtil.isEmpty(iFileService.getById(form.getSpuCoverFileId()))) {
            throw new GracefulResponseException("商品封面图片不存在！");
        }

        Spu spu = new Spu();
        BeanUtil.copyProperties(form, spu);

        try {
            iSpuService.save(spu);
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("商品名称重复！");
        }
    }

    @PutMapping(path = "/visible")
    @Operation(summary = "管理员更新商品是否可见", description = "管理员更新商品是否可见")
    public void putSpuVisible(@RequestBody @Valid UpdateSpuVisibleForm form) {
        Spu byId = iSpuService.getById(form.getSpuId());

        if (BeanUtil.isEmpty(byId)) {
            throw new GracefulResponseException("商品SPU不存在！");
        }

        byId.setVisible(form.getVisible());

        if (!iSpuService.updateById(byId)) {
            throw new ServerErrorException();
        }
    }

    @PostMapping(path = "/client/{categoryId}")
    @Operation(summary = "客户查询商品", description = "客户查询商品")
    @Parameter(name = "categoryId", description = "所属分类", example = "1", required = true)
    public PageResult<ClientSpuVo> list4Client(@RequestBody @Valid ConditionQuery form, @PathVariable Integer categoryId) {
        if (BeanUtil.isEmpty(iSpuCategoryService.getById(categoryId))) {
            throw new GracefulResponseException("商品所属分类不存在！");
        }

        PageResult<ClientSpuVo> result = new PageResult<>();
        result.setTotal(iSpuService.lambdaQuery().eq(Spu::getCategoryId, categoryId).eq(Spu::getVisible, Boolean.TRUE).count());
        iSpuService.lambdaQuery().eq(Spu::getCategoryId, categoryId).eq(Spu::getVisible, Boolean.TRUE).page(new Page<>(form.getCurrent(), form.getLimit())).getRecords().forEach(spu -> {
            ClientSpuVo clientSpuVo = new ClientSpuVo();
            BeanUtil.copyProperties(spu, clientSpuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileService.getById(spu.getSpuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                clientSpuVo.setSpuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(clientSpuVo);
        });
        return result;
    }

    @PostMapping(path = "/list/{categoryId}")
    @Operation(summary = "查询商品（管理员）", description = "查询商品（管理员）")
    public PageResult<SpuVo> list(@RequestBody ConditionQuery query, @PathVariable Integer categoryId) {
        QueryWrapper<Spu> wrapper = QueryUtil.buildWrapper(query, Spu.class).eq("category_id", categoryId);
        if (BeanUtil.isEmpty(iSpuCategoryService.getById(categoryId))) {
            throw new GracefulResponseException("商品所属分类不存在！");
        }
        PageResult<SpuVo> result = new PageResult<>();
        result.setTotal(iSpuService.count(wrapper));

        iSpuService.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(spu -> {
            SpuVo spuVo = new SpuVo();
            BeanUtil.copyProperties(spu, spuVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileService.getById(spu.getSpuCoverFileId());
            if (BeanUtil.isNotEmpty(file)) {
                spuVo.setSpuCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }
            result.getData().add(spuVo);
        });
        return result;
    }
}
