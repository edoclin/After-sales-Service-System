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
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.repository.IMaterialLogRepository;
import com.mi.aftersales.repository.ISpuCategoryRepository;
import com.mi.aftersales.service.MaterialService;
import com.mi.aftersales.repository.IMaterialRepository;
import com.mi.aftersales.service.SpuCategoryService;
import com.mi.aftersales.util.COSUtil;
import com.mi.aftersales.util.DateUtil;
import com.mi.aftersales.util.query.ConditionQuery;
import com.mi.aftersales.util.query.QueryUtil;
import com.mi.aftersales.util.query.enums.Operator;
import com.mi.aftersales.pojo.common.PageResult;
import com.mi.aftersales.pojo.vo.form.ManngerUpdateMaterialFormVo;
import com.mi.aftersales.pojo.vo.form.MaterialFormVo;
import com.mi.aftersales.pojo.vo.MaterialLogVo;
import com.mi.aftersales.pojo.vo.MaterialVo;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static com.mi.aftersales.util.RocketMqTopic.ROCKETMQ_TOPIC_4_MATERIAL_LOG;

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
    private RocketMQTemplate rocketmqTemplate;

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

                    .setLogDetail("添加新物料入库").setOperatorId(StpUtil.getLoginIdAsString()).setAction(MaterialActionEnum.NEW).setDelta(material.getStock()).setCreatedId(StpUtil.getLoginIdAsString());

            Message<List<MaterialLog>> msg = MessageBuilder.withPayload(CollUtil.list(Boolean.FALSE, materialLog)).build();
            rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_MATERIAL_LOG, msg);

        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        } catch (DuplicateKeyException e) {
            throw new GracefulResponseException("物料名称重复！");
        } catch (BaseCustomException | GracefulResponseException e) {
            throw e;
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

    @Override
    public void updateMaterial(ManngerUpdateMaterialFormVo form) {
        Material material = iMaterialRepository.getById(form.getMaterialId());

        if (BeanUtil.isEmpty(material)) {
            throw new IllegalMaterialIdException();
        }

        if (BeanUtil.isEmpty(iSpuCategoryRepository.getById(form.getSpuCategoryId()))) {
            throw new IllegalSpuCategoryIdException();
        }

        //
        RLock fairLock = null;
        try {
            fairLock = redissonClient.getFairLock(NAMESPACE_4_MATERIAL_LOCK + form.getMaterialId());
            if (fairLock.tryLock(30, TimeUnit.SECONDS)) {
                BeanUtil.copyProperties(form, material, CopyOptions.create().ignoreNullValue());

                if (material.getAlertNum().compareTo(material.getStock()) > 0) {
                    throw new GracefulResponseException("库存告警阈值不能大于物料剩余库存!");
                }
                if (material.getCost().compareTo(material.getPrice()) > 0) {
                    throw new GracefulResponseException("物料成本不能大于销售价格!");
                }
                if (Boolean.FALSE.equals(iMaterialRepository.updateById(material))) {
                    throw new ServerErrorException();
                }

                // 日志
                MaterialLog materialLog = new MaterialLog();
                materialLog
                        .setMaterialId(material.getMaterialId())
                        .setLogDetail("修改物料信息")
                        .setOperatorId(StpUtil.getLoginIdAsString())
                        .setAction(MaterialActionEnum.UPDATE)
                        .setDelta(material.getStock())
                        .setCreatedId(StpUtil.getLoginIdAsString());

                Message<List<MaterialLog>> msg = MessageBuilder.withPayload(CollUtil.list(Boolean.FALSE, materialLog)).build();
                rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_MATERIAL_LOG, msg);

            }
        } catch (ConvertException e) {
            throw new GracefulResponseException("物料类型不合法!");
        } catch (BaseCustomException e) {
            throw e;
        } catch (InterruptedException e) {
            // 加锁失败
            throw new ServerErrorException();
        } finally {
            if (null != fairLock) {
                fairLock.unlock();
            }
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

        iMaterialLogRepository.lambdaQuery().eq(MaterialLog::getMaterialId, material.getMaterialId()).list().forEach(log -> {
            MaterialLogVo logVo = new MaterialLogVo();
            BeanUtil.copyProperties(log, logVo, DateUtil.copyDate2yyyyMMddHHmm());
            logVo.setAction(log.getAction().getDesc());
            logVo.setDelta(log.getDelta().stripTrailingZeros().toEngineeringString());
            result.getLogs().add(logVo);
        });

        return result;
    }

    @Override
    public PageResult<MaterialVo> conditionQuery(ConditionQuery query) {
        QueryWrapper<Material> wrapper = QueryUtil.buildWrapper(query, Material.class);
        PageResult<MaterialVo> result = new PageResult<>();

        List<Integer> in = new ArrayList<>();

        query.getParams().forEach(param -> {
            if (param.getOperator() == Operator.CUSTOM) {
                in.addAll(spuCategoryService.childrenCategoryId(Integer.valueOf(param.getValue())));
            }
        });

        wrapper = wrapper.in("spu_category_id", in);
        result.setTotal(iMaterialRepository.count(wrapper));

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

        try {
            iMaterialRepository.removeById(materialId);

            // 日志
            MaterialLog materialLog = new MaterialLog();
            materialLog
                    .setMaterialId(materialId)
                    .setLogDetail("删除物料")
                    .setOperatorId(StpUtil.getLoginIdAsString())
                    .setAction(MaterialActionEnum.DELETE)
                    .setDelta(material.getStock())
                    .setCreatedId(StpUtil.getLoginIdAsString());

            Message<List<MaterialLog>> msg = MessageBuilder.withPayload(CollUtil.list(Boolean.FALSE, materialLog)).build();
            rocketmqTemplate.syncSend(ROCKETMQ_TOPIC_4_MATERIAL_LOG, msg);
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new ServerErrorException();
        }
    }

}
