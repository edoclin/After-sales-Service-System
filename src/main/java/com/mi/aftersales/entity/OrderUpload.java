package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;
import java.time.LocalDateTime;

import com.mi.aftersales.enums.entity.OrderUploadFileTypeEnum;
import com.mi.aftersales.enums.entity.OrderUploaderTypeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 工单的附属文件
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_order_upload")
@Schema(name = "OrderUpload", description = "工单的附属文件")
public class OrderUpload implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    @TableField(fill = FieldFill.UPDATE)
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    @TableField(fill = FieldFill.INSERT)
    private String createdId = "";

    @Schema(description = "更新者")
    @TableField(fill = FieldFill.UPDATE)
    private String updatedId = "";

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "对应文件ID")
    @TableId(value = "file_id", type = IdType.INPUT)
    private String fileId;

    @Schema(description = "上传者类型;1:客户上传;2:工程师上传")
    private OrderUploaderTypeEnum uploaderType;

    @Schema(description = "所属订单ID")
    private String orderId;

    @Schema(description = "文件类型;1:图片;2:视频")
    private OrderUploadFileTypeEnum fileType;
}
