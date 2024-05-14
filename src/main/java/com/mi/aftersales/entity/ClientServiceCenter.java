package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.time.LocalDateTime;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * <p>
 * 客户服务中心
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_client_service_center")
@Schema(name = "ClientServiceCenter", description = "客户服务中心")
public class ClientServiceCenter implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "创建时间")
    private LocalDateTime createdTime;

    @Schema(description = "更新时间")
    private LocalDateTime updatedTime;

    @Schema(description = "创建者")
    private String createdId;

    @Schema(description = "更新者")
    private String updatedId;

    @Schema(description = "逻辑删除")
    @TableLogic
    private Long deleted;

    @Schema(description = "地址ID")
    @TableId(value = "center_id", type = IdType.AUTO)
    private String centerId;

    @Schema(description = "省市区")
    private String region;

    @Schema(description = "详细地址")
    private String addressDetail;

    @Schema(description = "中心详情")
    private String centerDesc;

    @Schema(description = "联系方式")
    private String mobile;

    @Schema(description = "服务中心名称")
    private String centerName;
}
