package com.mi.aftersales.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serializable;
import java.time.LocalDateTime;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@ApiModel(value = "ClientServiceCenter对象", description = "客户服务中心")
public class ClientServiceCenter implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("创建时间")
    private LocalDateTime createdTime;

    @ApiModelProperty("更新时间")
    private LocalDateTime updatedTime;

    @ApiModelProperty("创建者")
    private String createdId;

    @ApiModelProperty("更新者")
    private String updatedId;

    @ApiModelProperty("逻辑删除")
    @TableLogic
    private Long deleted;

    @ApiModelProperty("地址ID")
    @TableId(value = "center_id", type = IdType.AUTO)
    private String centerId;

    @ApiModelProperty("省市区")
    private String region;

    @ApiModelProperty("详细地址")
    private String addressDetail;

    @ApiModelProperty("中心详情")
    private String centerDesc;

    @ApiModelProperty("联系方式")
    private String mobile;

    @ApiModelProperty("服务中心名称")
    private String centerName;
}
