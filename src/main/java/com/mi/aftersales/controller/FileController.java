package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.repository.IFileRepository;
import com.mi.aftersales.service.FileService;
import com.mi.aftersales.vo.result.FileUploadVo;
import com.mi.aftersales.vo.form.FileForm;
import io.swagger.v3.oas.annotations.Operation;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 对象存储 前端控制器
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@RestController
@RequestMapping("/aftersales/file")
public class FileController {
    @Resource
    private FileService fileService;

    @PostMapping(path = "/upload")
    @CheckLogin
    @Operation(summary = "保存COS的accessKey并返回文件Id", description = "保存COS的accessKey并返回文件Id")
    public List<FileUploadVo> postFile(@RequestBody @Valid FileForm form) {
        return fileService.postFile(form);
    }
}
