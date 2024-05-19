package com.mi.aftersales.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;


/**
 * @description: 跨域配置
 * @return:
 * @author: edoclin
 * @created: 2024/5/19 19:08
 **/
@Configuration
@EnableWebMvc
public class CorsConfiguration implements WebMvcConfigurer {
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry
                //设置允许跨域的路径
                .addMapping("/**")
                //设置允许跨域请求的域名
                .allowedOriginPatterns("*")
                //这里：是否允许证书 不再默认开启
                .allowCredentials(true)
                //设置允许的方法
                .allowedMethods("*")
                //跨域允许时间
                .maxAge(3600);
    }
}
