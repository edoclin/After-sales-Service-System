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
 * 发票信息
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_fapiao")
@ApiModel(value = "Fapiao对象", description = "发票信息")
public class Fapiao implements Serializable {

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

    @ApiModelProperty("发票ID")
    @TableId(value = "fapiao_id", type = IdType.AUTO)
    private String fapiaoId;

    @ApiModelProperty("发票号码")
    private String fapiaoNo;

    @ApiModelProperty("发票代码")
    private String fapiaoCode;

    @ApiModelProperty("发票信息(JSON)")
    private String fapiaoInfo;

    @ApiModelProperty("开票时间")
    private LocalDateTime fapiaoTime;
}
