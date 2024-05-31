package com.mi.aftersales.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.service.FileService;
import com.mi.aftersales.vo.form.FileForm;
import com.mi.aftersales.vo.result.FileUploadVo;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestBody;

import javax.annotation.Resource;
import javax.validation.Valid;
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
    @Resource
    private IFileRepository iFileRepository;

    @Transactional
    @Override
    public List<FileUploadVo> postFile(@RequestBody @Valid FileForm form) {
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
}
