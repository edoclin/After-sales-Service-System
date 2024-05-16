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
 * 工单评价
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@TableName("t_rating")
@Schema(name = "Rating", description = "工单评价")
public class Rating implements Serializable {

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

    @Schema(description = "评价ID")
    @TableId(value = "rating_id", type = IdType.AUTO)
    private String ratingId;

    @Schema(description = "工单ID")
    private String orderId;

    @Schema(description = "评分")
    private Integer rating;

    @Schema(description = "评价类型")
    private Byte ratingType;

    @Schema(description = "评价内容(JSON:图片ID数组和文字)")
    private String content;
}
