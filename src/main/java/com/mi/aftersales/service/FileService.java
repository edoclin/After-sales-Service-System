package com.mi.aftersales.service;

import com.mi.aftersales.vo.form.FileForm;
import com.mi.aftersales.vo.result.FileUploadVo;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestBody;

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

    @Transactional
    List<FileUploadVo> postFile(@RequestBody @Valid FileForm form);
}
