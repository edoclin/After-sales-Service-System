package com.mi.aftersales;

import com.feiniaojin.gracefulresponse.EnableGracefulResponse;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @author edoclin
 */
@EnableGracefulResponse
@SpringBootApplication
@MapperScan("com.mi.aftersales.mapper")
public class AftersalesServiceSystem {
    public static void main(String[] args) {
        SpringApplication.run(AftersalesServiceSystem.class, args);
    }

}
