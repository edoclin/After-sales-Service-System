FROM openjdk:17
MAINTAINER edoclin
RUN mkdir -p /server/bin
RUN mkdir -p /server/logs
WORKDIR /server/bin
COPY ./bin/ /server/bin/
RUN cd /server/bin
RUN chmod -R 777 *
#对外暴露的端口
EXPOSE 8000

ARG profile

#程序启动脚本
CMD ["sh", "-c", "java -jar `ls` --spring.profiles.active=${profile} > /server/logs/console.log 2>&1"]