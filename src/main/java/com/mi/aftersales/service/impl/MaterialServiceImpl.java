package com.mi.aftersales.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.ConvertException;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.entity.Material;
import com.mi.aftersales.entity.MaterialLog;
import com.mi.aftersales.enums.entity.MaterialActionEnum;
import com.mi.aftersales.exception.graceful.BaseCustomException;
import com.mi.aftersales.exception.graceful.IllegalMaterialIdException;
import com.mi.aftersales.exception.graceful.IllegalSpuCategoryIdException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.mq.producer.MqProducer;
import com.mi.aftersales.common.PageResult;
import com.mi.aftersales.pojo.vo.MaterialLogVo;
import com.mi.aftersales.pojo.vo.MaterialVo;
import com.mi.aftersales.pojo.vo.form.ManagerUpdateMaterialFormVo;
import com.mi.aftersales.pojo.vo.form.MaterialFormVo;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.repository.IMaterialLogRepository;
import com.mi.aftersales.repository.IMaterialRepository;
import com.mi.aftersales.repository.ISpuCategoryRepository;
import com.mi.aftersales.service.MaterialService;
import com.mi.aftersales.service.SpuCategoryService;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryParam;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.util.query.enums.Operator;
import com.mi.aftersales.util.view.ViewUtil;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.mi.aftersales.common.RocketMqTopic.ROCKETMQ_TOPIC_4_MATERIAL_LOG;

/**
 * <p>
 * 物料 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class MaterialServiceImpl implements MaterialService {

    private static final Logger log = LoggerFactory.getLogger(MaterialServiceImpl.class);

    @Resource
    private IMaterialRepository iMaterialRepository;


    @Resource
    private RedissonClient redissonClient;

    @Resource
    private ISpuCategoryRepository iSpuCategoryRepository;


    @Resource
    private IMaterialLogRepository iMaterialLogRepository;

    @Resource
    private IFileRepository iFileRepository;

    @Resource
    private MqProducer mqProducer;

    @Resource
    private SpuCategoryService spuCategoryService;

    @Override
    public void addMaterial(MaterialFormVo form) {
        if (form.getAlertNum().compareTo(form.getStock()) > 0) {
            throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
        }

        if (form.getCost().compareTo(form.getPrice()) > 0) {
            throw new GracefulResponseException("物料成本不能大于销售价格!");
        }

        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(form.getSpuCategoryId()))) {
            throw new IllegalSpuCategoryIdException();
        }

        Material material = new Material();
        try {
            BeanUtil.copyProperties(form, material);
            if (Boolean.FALSE.equals(iMaterialRepository.save(material))) {
                throw new ServerErrorException();
            }

            // 添加日志
            MaterialLog materialLog = new MaterialLog();
            materialLog
                    .setMaterialId(material.getMaterialId())
                    .setLogDetail("添加新物料入库")
                    .setOperatorId(StpUtil.getLoginIdAsString())
                    .setAction(MaterialActionEnum.NEW)
                    .setDelta(material.getStock())
                    .setCreatedId(StpUtil.getLoginIdAsString());

            mqProducer.asyncSend(ROCKETMQ_TOPIC_4_MATERIAL_LOG, CollUtil.list(Boolean.FALSE, materialLog));
        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        } catch (BaseCustomException | GracefulResponseException e) {
            throw e;
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @Override
    public void updateMaterial(ManagerUpdateMaterialFormVo form) {
        Material material = iMaterialRepository.getById(form.getMaterialId());

        if (BeanUtil.isEmpty(material)) {
            throw new IllegalMaterialIdException();
        }

        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(form.getSpuCategoryId()))) {
            throw new IllegalSpuCategoryIdException();
        }


        if (form.getAlertNum().compareTo(form.getStock()) > 0) {
            throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
        }
        if (form.getCost().compareTo(form.getPrice()) > 0) {
            throw new GracefulResponseException("物料成本不能大于销售价格!");
        }

        RLock fairLock = redissonClient.getFairLock(NAMESPACE_4_MATERIAL_LOCK + form.getMaterialId());

        if (fairLock == null) {
            throw new ServerErrorException();
        }

        MaterialLog materialLog = null;
        try {
            if (fairLock.tryLock(30, TimeUnit.SECONDS)) {
                BeanUtil.copyProperties(form, material, CopyOptions.create().ignoreNullValue());

                iMaterialRepository.updateById(material);
                // 日志
                materialLog = new MaterialLog();
                materialLog
                        .setMaterialId(material.getMaterialId())
                        .setLogDetail("修改物料信息")
                        .setOperatorId(StpUtil.getLoginIdAsString())
                        .setAction(MaterialActionEnum.UPDATE)
                        .setDelta(material.getStock())
                        .setCreatedId(StpUtil.getLoginIdAsString());
            }
        } catch (InterruptedException e) {
            // 加锁失败
            log.error(e.getMessage());
            Thread.currentThread().interrupt();
            throw new ServerErrorException();
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        } finally {
            fairLock.unlock();
        }
        if (BeanUtil.isNotEmpty(materialLog)) {
            mqProducer.asyncSend(ROCKETMQ_TOPIC_4_MATERIAL_LOG, CollUtil.list(Boolean.FALSE, materialLog));
        }

    }

    @Override
    public MaterialVo getMaterialDetailById(String materialId) {
        Material material = iMaterialRepository.getById(materialId);

        if (BeanUtil.isEmpty(material)) {
            throw new IllegalMaterialIdException();
        }

        MaterialVo result = new MaterialVo();
        BeanUtil.copyProperties(material, result, DateUtil.copyDate2yyyyMMddHHmm());

        File file = iFileRepository.getById(material.getMaterialCoverFileId());

        if (BeanUtil.isNotEmpty(file)) {
            result.setCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
        }

        iMaterialLogRepository.lambdaQuery().eq(MaterialLog::getMaterialId, material.getMaterialId()).list().forEach(innerLog -> {
            MaterialLogVo logVo = new MaterialLogVo();
            BeanUtil.copyProperties(innerLog, logVo, DateUtil.copyDate2yyyyMMddHHmm());
            logVo.setAction(innerLog.getAction().getDesc());
            logVo.setDelta(innerLog.getDelta().stripTrailingZeros().toEngineeringString());
            result.getLogs().add(logVo);
        });

        return result;
    }

    @Override
    public PageResult<MaterialVo> conditionQuery(ConditionQuery query) {
        QueryWrapper<Material> wrapper = QueryUtil.buildWrapper(query, Material.class);
        PageResult<MaterialVo> result = new PageResult<>();

        for (QueryParam param : query.getParams()) {
            if (param.getOperator() == Operator.CUSTOM) {
                List<Integer> in = new ArrayList<>(spuCategoryService.childrenCategoryId(Integer.valueOf(param.getValue())));

                if (CollUtil.isNotEmpty(in)) {
                    wrapper = wrapper.in("spu_category_id", in);
                } else {
                    log.warn("非法的Spu分类Id：{}", param.getValue());
                }
                break;
            }
        }


        result.setTotal(iMaterialRepository.count(wrapper));
        result.setDataColumns(ViewUtil.dataColumns(MaterialVo.class));

        iMaterialRepository.page(new Page<>(query.getCurrent(), query.getLimit()), wrapper).getRecords().forEach(material -> {

            MaterialVo materialVo = new MaterialVo();
            BeanUtil.copyProperties(material, materialVo, DateUtil.copyDate2yyyyMMddHHmm());

            File file = iFileRepository.getById(material.getMaterialCoverFileId());

            if (BeanUtil.isNotEmpty(file)) {
                materialVo.setCoverUrl(COSUtil.generateAccessUrl(file.getAccessKey()));
            }

            result.getData().add(materialVo);
        });
        return result;
    }

    @Override
    public void deleteMaterialById(String materialId) {
        Material material = iMaterialRepository.getById(materialId);

        if (BeanUtil.isEmpty(material)) {
            throw new IllegalMaterialIdException();
        }
        MaterialLog materialLog;
        try {
            iMaterialRepository.removeById(materialId);

            // 日志
            materialLog = new MaterialLog();
            materialLog
                    .setMaterialId(materialId)
                    .setLogDetail("删除物料")
                    .setOperatorId(StpUtil.getLoginIdAsString())
                    .setAction(MaterialActionEnum.DELETE)
                    .setDelta(material.getStock())
                    .setCreatedId(StpUtil.getLoginIdAsString());
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }

        if (BeanUtil.isNotEmpty(materialLog)) {
            mqProducer.asyncSend(ROCKETMQ_TOPIC_4_MATERIAL_LOG, CollUtil.list(Boolean.FALSE, materialLog));
        }
    }

}
