package com.mi.aftersales.controller;

import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.pojo.vo.form.FileFormVo;
import com.mi.aftersales.service.FileService;
import com.mi.aftersales.pojo.vo.FileUploadVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.validation.Valid;
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
    @Operation(summary = "保存COS的accessKey并返回文件Id", description = "保存COS的accessKey并返回文件Id")
    @CheckLogin
    public List<FileUploadVo> postFile(@RequestBody @Valid FileFormVo form) {
        return fileService.generateFileIds(form);
    }


    @PostMapping(path = "/upload/by-server")
    @CheckLogin
    @Operation(summary = "上传到服务器再上传到COS", description = "上传到服务器再上传到COS")
    @Parameter(name = "files", description = "二进制文件数组")
    public List<FileUploadVo> uploadByServer(@RequestBody MultipartFile[] files) {
        return fileService.uploadByServer(files);
    }
}
