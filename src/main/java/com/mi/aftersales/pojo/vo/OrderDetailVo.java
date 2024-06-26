package com.mi.aftersales.pojo.vo;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

/**
 * @author edoclin
 * @since 2024-05-14
 */
@Getter
@Setter
@Accessors(chain = true)
@Schema(title = "工单信息（详情）", description = "工单信息（详情）")
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class OrderDetailVo {
    @Schema(description = "工单Id")
    private String orderId;
    @Schema(description = "产品序列号")
    private String sn;
    @Schema(description = "下单时间")
    private String createdTime;
    @Schema(description = "更新时间")
    private String updatedTime;
    @Schema(description = "取件/到店时间")
    private String arrivalTime;
    @Schema(description = "工单状态")
    private String orderStatus;
    private Integer orderStatusValue;
    @Schema(description = "故障描述")
    private String clientFaultDesc;
    private ClientSkuVo sku;
    private ClientFapiaoVo fapiao;
    private ClientAddressVo address;
    @Schema(description = "预估物料费用")
    private String materialFee;
    @Schema(description = "预估手工费用")
    private String manualFee;
    @Schema(description = "工程师故障描述")
    private String engineerFaultDesc;
    @Schema(description = "工程师备注")
    private String engineerNotice;

    @Schema(description = "客户上传文件")
    private List<FileVo> clientFileUrl = new ArrayList<>();

    @Schema(description = "工程师上传图片文件")
    private List<FileVo> engineerImageUrl = new ArrayList<>();

    @Schema(description = "工程师上传视频文件")
    private List<FileVo> engineerVideoUrl = new ArrayList<>();

    private List<ClientOrderStatusLogVo> statusLogs = new ArrayList<>();
}
