package com.mi.aftersales.mapper;

import com.mi.aftersales.entity.LoginPermission;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * <p>
 * 用户具有权限中间表 Mapper 接口
 * </p>
 *
 * @author edoclin
 * @since 2024-05-14
 */
@Mapper
public interface MiddleLoginPermissionMapper extends BaseMapper<LoginPermission> {

}
