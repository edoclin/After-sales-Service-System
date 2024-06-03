package com.mi.aftersales.service;

import com.mi.aftersales.pojo.vo.form.FileFormVo;
import com.mi.aftersales.pojo.vo.FileUploadVo;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * <p>
 * 对象存储 服务类
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
public interface FileService {


    List<FileUploadVo> generateFileIds(FileFormVo form);

    List<FileUploadVo>  uploadByServer(MultipartFile[] files);
}
