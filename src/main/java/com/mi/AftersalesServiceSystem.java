package com.mi;

import com.feiniaojin.gracefulresponse.EnableGracefulResponse;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @author edoclin
 */
@EnableGracefulResponse
@SpringBootApplication
public class AftersalesServiceSystem {

    public static void main(String[] args) {
        SpringApplication.run(AftersalesServiceSystem.class, args);
    }

}
