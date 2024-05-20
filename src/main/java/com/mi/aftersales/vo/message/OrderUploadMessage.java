package com.mi.aftersales.vo.message;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.mi.aftersales.entity.enums.LoginTypeEnum;
import com.mi.aftersales.entity.enums.OrderUploaderTypeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @description:
 * @return:
 * @author: edoclin
 * @created: 2024/5/20 14:39
 **/
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "工单文件消息", description = "工单文件消息")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class OrderUploadMessage {
    @Schema(description = "工单Id")
    private String orderId;
    @Schema(description = "文件Ids")
    private String[] fileIds;

    @Schema(description = "上传者类型")
    private OrderUploaderTypeEnum uploaderType;
}
