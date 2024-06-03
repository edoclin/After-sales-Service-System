package com.mi.aftersales.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.IdUtil;
import com.mi.aftersales.config.yaml.bean.TencentCosConfig;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import com.mi.aftersales.pojo.vo.FileUploadVo;
import com.mi.aftersales.pojo.vo.form.FileFormVo;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.service.FileService;
import com.qcloud.cos.COSClient;
import com.qcloud.cos.ClientConfig;
import com.qcloud.cos.auth.BasicCOSCredentials;
import com.qcloud.cos.auth.COSCredentials;
import com.qcloud.cos.model.PutObjectRequest;
import com.qcloud.cos.model.UploadResult;
import com.qcloud.cos.region.Region;
import com.qcloud.cos.transfer.TransferManager;
import net.coobird.thumbnailator.Thumbnails;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 对象存储 服务实现类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Service
public class FileServiceImpl implements FileService {
    private static final Logger log = LoggerFactory.getLogger(FileServiceImpl.class);
    @Resource
    private IFileRepository iFileRepository;

    @Resource
    private TencentCosConfig cosConfig;


    @Override
    public List<FileUploadVo> generateFileIds(FileFormVo form) {
        ArrayList<FileUploadVo> result = new ArrayList<>();
        ArrayList<File> files = new ArrayList<>();

        for (String key : form.getKeys()) {
            files.add(new File().setAccessKey(key).setUploaderId(StpUtil.getLoginIdAsString()));
        }

        if (iFileRepository.saveBatch(files)) {
            files.forEach(file -> result.add(new FileUploadVo().setFileId(file.getFileId()).setAccessKey(file.getAccessKey())));
        }

        return result;
    }

    @Override
    public List<FileUploadVo> uploadByServer(MultipartFile[] files) {
        FileFormVo fileFormVo = new FileFormVo();

        ClientConfig clientConfig = new ClientConfig(new Region(cosConfig.getRegion()));
        COSCredentials cred = new BasicCOSCredentials(cosConfig.getSecretId(), cosConfig.getSecretKey());
        COSClient cosClient = new COSClient(cred, clientConfig);
        TransferManager transferManager = new TransferManager(cosClient);

        try {

            for (MultipartFile file : files) {

                String key = CharSequenceUtil.format("{}_{}", Long.toHexString(IdUtil.getSnowflakeNextId()), file.getOriginalFilename());
                java.io.File localFile = new java.io.File(cosConfig.getTempPath(), key);
                file.transferTo(localFile);

                java.io.File uploadFile = new java.io.File(cosConfig.getTempPath(), "zip_" + key);
                Thumbnails.of(localFile).scale(.1f).outputQuality(.35f).toFile(uploadFile);
                PutObjectRequest putObjectRequest = new PutObjectRequest(cosConfig.getBucketName(), cosConfig.getPrefix() + key, uploadFile);
                UploadResult uploadResult = transferManager.upload(putObjectRequest).waitForUploadResult();
                fileFormVo.getKeys().add(uploadResult.getKey());
                if (Boolean.FALSE.equals(FileUtil.del(localFile)) || Boolean.FALSE.equals(FileUtil.del(uploadFile))) {
                    log.warn("缓存文件删除失败：{}", key);
                }

            }
        } catch (IOException e) {
            log.error("文件转换失败！");
            log.error(e.getMessage());
            throw new ServerErrorException();
        } catch (InterruptedException e) {
            log.error("文件上传失败！");
            log.error(e.getMessage());
            Thread.currentThread().interrupt();
            throw new ServerErrorException();
        }
        return generateFileIds(fileFormVo);
    }
}
