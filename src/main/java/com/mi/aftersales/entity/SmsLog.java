package com.mi.aftersales.entity;

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
 * 短信推送日志
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_sms_log")
@ApiModel(value = "SmsLog对象", description = "短信推送日志")
public class SmsLog implements Serializable {

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

    @ApiModelProperty("日志ID")
    private String logId;

    @ApiModelProperty("推送类型")
    private Byte smsType;

    @ApiModelProperty("目标手机号")
    private String mobile;

    @ApiModelProperty("推送结果")
    private Byte result;

    @ApiModelProperty("响应结果")
    private String response;

    @ApiModelProperty("推送详情")
    private String detail;
}
