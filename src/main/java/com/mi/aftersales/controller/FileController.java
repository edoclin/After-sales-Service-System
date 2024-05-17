package com.mi.aftersales.controller;

import cn.dev33.satoken.stp.StpUtil;
import com.mi.aftersales.aspect.anno.CheckLogin;
import com.mi.aftersales.entity.File;
import com.mi.aftersales.service.IFileService;
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
    private IFileService fileService;

    @PostMapping(path = "/upload")
    @CheckLogin
    @Operation(summary = "当前用户删除发票信息", description = "当前用户删除发票信息")
    public List<FileUploadVo> postFile(@RequestBody @Valid FileForm form) {
        ArrayList<FileUploadVo> result = new ArrayList<>();
        ArrayList<File> files = new ArrayList<>();

        for (String key : form.getKeys()) {
            files.add(new File().setAccessKey(key).setUploaderId(StpUtil.getLoginIdAsString()));
        }

        if (fileService.saveBatch(files)) {
            files.forEach(file -> result.add(new FileUploadVo().setFileId(file.getFileId()).setAccessKey(file.getAccessKey())));
        }

        return result;
    }
}
