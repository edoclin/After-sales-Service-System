package com.mi.aftersales.vo;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableLogic;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.time.LocalDateTime;

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
@ApiModel(value = "LoginVo对象", description = "")
// 返回时忽略空值
@JsonInclude(JsonInclude.Include.NON_EMPTY)
public class LoginVo {
    @ApiModelProperty("Token名称")
    private String tokenName;
    @ApiModelProperty("Token值")
    private String tokenValue;
    @ApiModelProperty("用户ID")
    private String loginId;
    @ApiModelProperty("其他信息")
    private String note;


}
