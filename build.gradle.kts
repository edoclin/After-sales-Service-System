plugins {
    java
    id("org.springframework.boot") version "2.7.6"
    id("io.spring.dependency-management") version "1.0.11.RELEASE"
    id("jacoco")
    id("org.sonarqube") version "4.0.0.2929"
}

group = "com.mi"
version = "0.0.1-SNAPSHOT"

val hutoolVersion = "5.8.27"
val saTokenVersion = "1.38.0"
val justAuthVersion = "1.16.6"
val mybatisPlusVersion = "3.5.6"
val gracefulResponseVersion = "3.5.2-boot2"
val jedisVersion = "5.1.2"
val alipaySdkVersion = "4.35.87.ALL"
val cosApiSdkVersion = "5.6.213"
val statemachineVersion = "4.0.0"
val springfoxVersion = "3.0.0"
val freemarkerVersion = "2.3.31"
val redissonVersion = "3.18.0"
val springDocVersion = "1.8.0"
val sms4jVersion = "3.2.1"
val rocketmqVersion = "2.3.0"
val mockitoCoreVersion = "4.8.0"
val mockitoInlineVersion = "4.8.0"
val junitJupiterApiVersion = "5.7.2"
val junitJupiterEngineVersion = "5.7.2"
val lettuceVersion = "6.3.2.RELEASE"
val thumbnailatorVersion = "0.4.20"

java {
    sourceCompatibility = JavaVersion.VERSION_17
    targetCompatibility = JavaVersion.VERSION_17
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
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

    implementation("org.redisson:redisson-spring-boot-starter:$redissonVersion")

    implementation("org.springdoc:springdoc-openapi-ui:$springDocVersion")

    implementation("org.dromara.sms4j:sms4j-spring-boot-starter:$sms4jVersion")
    implementation("org.springframework.boot:spring-boot-starter-aop")

    implementation("org.apache.rocketmq:rocketmq-spring-boot-starter:$rocketmqVersion")

    // 状态机
    implementation("org.springframework.statemachine:spring-statemachine-core:$statemachineVersion")
    implementation("org.springframework.statemachine:spring-statemachine-data-redis:$statemachineVersion")

    implementation("cn.hutool:hutool-all:$hutoolVersion")
    implementation("cn.dev33:sa-token-spring-boot-starter:$saTokenVersion")
    implementation("cn.dev33:sa-token-redis-jackson:$saTokenVersion")
    implementation("cn.dev33:sa-token-alone-redis:$saTokenVersion")
    implementation("com.baomidou:mybatis-plus-boot-starter:$mybatisPlusVersion")
    implementation("com.baomidou:mybatis-plus-generator:$mybatisPlusVersion")
    implementation("org.freemarker:freemarker:$freemarkerVersion")
    implementation("me.zhyd.oauth:JustAuth:$justAuthVersion")
    implementation("com.feiniaojin:graceful-response:$gracefulResponseVersion")
    implementation("redis.clients:jedis:$jedisVersion")
    // 腾讯云文件存储服务
    implementation("com.qcloud:cos_api:$cosApiSdkVersion")
    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")
    runtimeOnly("mysql:mysql-connector-java")

    implementation("io.lettuce:lettuce-core:$lettuceVersion")

    testImplementation("org.springframework.boot:spring-boot-starter-test")
    // Mockito核心库
    testImplementation("org.mockito:mockito-core:$mockitoCoreVersion")
    // 如果需要Mockito与Spring集成的扩展功能，可以使用mockito-spring库
    testImplementation("org.mockito:mockito-inline:$mockitoInlineVersion")  // 支持内联Mock

    // JUnit 5依赖
    testImplementation("org.junit.jupiter:junit-jupiter-api:$junitJupiterApiVersion")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:$junitJupiterEngineVersion")

    implementation("net.coobird:thumbnailator:$thumbnailatorVersion")

}

tasks.withType<Test> {
    useJUnitPlatform()
    finalizedBy(tasks.jacocoTestReport)//确保在测试运行后生成 Jacoco 报告
}

jacoco {
    toolVersion = "0.8.7"
}

tasks.jacocoTestReport {
    dependsOn(tasks.test)
    reports {
        xml.required.set(true)
        html.required.set(true)
    }
}


sonarqube {
    properties {
        property ("sonar.projectKey", "my-project")
        property ("sonar.host.url", "http://localhost:9000")
        property ("sonar.login", "your_sonar_token") // Replace with your actual token
        property ("sonar.language", "java")
        property ("sonar.sources", "src/main/java")
        property ("sonar.tests", "src/test/java")
        property ("sonar.java.binaries", "build/classes/java/main")
        property ("sonar.junit.reportPaths", "build/test-results/test")
        property ("sonar.jacoco.reportPaths", "build/jacoco/test.exec")
    }
}