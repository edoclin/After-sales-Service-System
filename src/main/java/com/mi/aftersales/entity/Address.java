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
 * 客户联系地址
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_address")
@Schema(name = "Address", description = "客户联系地址")
public class Address implements Serializable {

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

    @Schema(description = "地址ID")
    @TableId(value = "address_id", type = IdType.AUTO)
    private String addressId;

    @Schema(description = "省市区")
    private String region;

    @Schema(description = "详细地址")
    private String addressDetail;

    @Schema(description = "客户ID")
    private String loginId;

    @Schema(description = "联系方式")
    private String mobile;

    @Schema(description = "是否默认地址")
    private Boolean defaulted;

    @Schema(description = "收货人姓名")
    private String receiver;
}
