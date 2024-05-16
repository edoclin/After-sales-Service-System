package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.time.LocalDateTime;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 对象存储
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_file")
@Schema(name = "File", description = "对象存储")
public class File implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    private String createdId;

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    private String updatedId;

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "对象存储ID")
    @TableId(value = "file_id", type = IdType.AUTO)
    private String fileId;

    @Schema(description = "对象访问路径")
    private String accessKey;

    @Schema(description = "上传者")
    private String uploaderId;
}
