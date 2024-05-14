plugins {
    java
    id("org.springframework.boot") version "2.7.6"
    id("io.spring.dependency-management") version "1.0.11.RELEASE"
}

group = "com.mi"
version = "0.0.1-SNAPSHOT"

val hutoolVersion = "5.8.27"
val saTokenVersion = "1.37.0"
val justAuthVersion = "1.16.6"
val mybatisPlusVersion = "3.5.6"
val gracefulResponseVersion = "3.5.2-boot2"
val jedisVersion = "5.1.2"
val alipaySdkVersion = "4.35.87.ALL"
val cosApiSdkVersion = "5.6.211"
val tencentCloudSdkVersion = "3.1.1013"
val statemachineVersion = "4.0.0"
val springfoxVersion = "3.0.0"
val freemarkerVersion = "2.3.31"
val redissonVersion = "3.18.0"
val springDocVersion = "1.8.0"

java {
    sourceCompatibility = JavaVersion.VERSION_17
}
configurations {
    compileOnly {
        extendsFrom(configurations.annotationProcessor.get())
    }
}


repositories {
    maven {
        setUrl("https://maven.aliyun.com/repository/public/")
    }
    mavenCentral()
}

dependencies {
    implementation("com.alipay.sdk:alipay-sdk-java:$alipaySdkVersion")
    // MybatisPlus代码生成模版引擎
//    implementation("org.springframework.boot:spring-boot-starter-validation")
// https://mvnrepository.com/artifact/org.redisson/redisson-spring-boot-starter
    implementation("org.redisson:redisson-spring-boot-starter:$redissonVersion")
// https://mvnrepository.com/artifact/org.springdoc/springdoc-openapi-ui
    implementation("org.springdoc:springdoc-openapi-ui:$springDocVersion")

    // 状态机
    implementation("org.springframework.statemachine:spring-statemachine-core:$statemachineVersion")
    implementation("cn.hutool:hutool-all:$hutoolVersion")
    implementation("cn.dev33:sa-token-spring-boot-starter:$saTokenVersion")
    implementation("cn.dev33:sa-token-redis-jackson:$saTokenVersion")
    implementation("com.baomidou:mybatis-plus-boot-starter:$mybatisPlusVersion")
    implementation("com.baomidou:mybatis-plus-generator:$mybatisPlusVersion")
    implementation("org.freemarker:freemarker:$freemarkerVersion")

    implementation("me.zhyd.oauth:JustAuth:$justAuthVersion")
    implementation("com.feiniaojin:graceful-response:$gracefulResponseVersion")
    implementation("redis.clients:jedis:$jedisVersion")
    // 腾讯与文件存储服务
    implementation("com.qcloud:cos_api:$cosApiSdkVersion")
    // 腾讯云短信服务
    implementation("com.tencentcloudapi:tencentcloud-sdk-java:$tencentCloudSdkVersion")
//    implementation("io.springfox:springfox-boot-starter:$springfoxVersion")

    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")

    runtimeOnly("mysql:mysql-connector-java")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
}

tasks.withType<Test> {
    useJUnitPlatform()
}
