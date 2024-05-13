package com.mi.aftersales;

import com.feiniaojin.gracefulresponse.EnableGracefulResponse;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import springfox.documentation.oas.annotations.EnableOpenApi;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * @author edoclin
 */
@EnableGracefulResponse
@SpringBootApplication
@MapperScan("com.mi.aftersales.mapper")

@EnableSwagger2
@EnableOpenApi
@EnableWebMvc
public class AftersalesServiceSystem {

    public static void main(String[] args) {
        SpringApplication.run(AftersalesServiceSystem.class, args);
    }

}
