package com.mi.aftersales.service;

import com.mi.aftersales.vo.form.FileForm;
import com.mi.aftersales.vo.result.FileUploadVo;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
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


    List<FileUploadVo> postFile(FileForm form);

    List<FileUploadVo>  uploadByServer(MultipartFile[] files);
}
