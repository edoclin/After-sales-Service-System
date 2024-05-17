package com.mi.aftersales.util;

/**
 * @description: 腾讯云COS工具
 * @return:
 * @author: edoclin
 * @created: 2024/5/17 19:12
 **/
public class COSUtil {
    public static final String DOMAIN = "https://dev-1300627747.cos.ap-chengdu.myqcloud.com";

    public static String generateAccessUrl(String accessKey) {
        return DOMAIN + accessKey;
    }
}
