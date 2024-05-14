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
@Schema(name = "Fapiao", description = "发票信息")
public class Fapiao implements Serializable {

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

    @Schema(description = "发票ID")
    @TableId(value = "fapiao_id", type = IdType.AUTO)
    private String fapiaoId;

    @Schema(description = "发票号码")
    private String fapiaoNo;

    @Schema(description = "发票代码")
    private String fapiaoCode;

    @Schema(description = "发票信息(JSON)")
    private String fapiaoInfo;

    @Schema(description = "开票时间")
    private LocalDateTime fapiaoTime;
}
