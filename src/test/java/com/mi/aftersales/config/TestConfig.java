package com.mi.aftersales.config;

import com.feiniaojin.gracefulresponse.GracefulResponseException;
import com.mi.aftersales.exception.graceful.BaseCustomException;
import com.mi.aftersales.exception.graceful.ServerErrorException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * @author QYenon
 * @create 2024/5/31
 */
@Component
public class TestConfig {

    private static final String token_value = "Bearer Es_w2FK3UZWtLGoR1_VWWUeDsFzGaTZlNR__";

    private static final Logger logger = LoggerFactory.getLogger(TestConfig.class);

    private MockMvc mockMvc;

    public MockMvc getMockMvc() {
        return mockMvc;
    }

    public void setMockMvc(MockMvc mockMvc) {
        this.mockMvc = mockMvc;
    }

    public void getMockMvcResult(String url) throws Exception {
        try {
            MvcResult mvcResult = mockMvc.perform(
                            MockMvcRequestBuilders.get(url)
                                    .header("aftersales-token", token_value)
                    )
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andDo(MockMvcResultHandlers.print())
                    .andReturn();
            String responseContent = mvcResult.getResponse().getContentAsString();
            System.out.println("调用返回的结果：" + responseContent);

            // 断言状态码为 200
            assertEquals(200, mvcResult.getResponse().getStatus());
        } catch (Exception e) {
            // 将异常信息打印到控制台，以便调试
            e.printStackTrace();

            if  (e.getCause() instanceof GracefulResponseException) {
                logger.error("捕获到自定义Graceful异常：{}", e.getCause().getMessage());
            }  else if (e.getCause() instanceof ServerErrorException) {
                logger.error("捕获到服务器繁忙异常：{}", e.getCause().getMessage());
            } else if (e.getCause() instanceof BaseCustomException) {
                logger.error("捕获到自定义基础异常：{}", e.getCause().getMessage());
            } else {
                fail("未捕获到预期的异常，捕获到其他异常：" + e.getCause().getMessage());
            }
        }
    }

    public void getMockMvcResult(String url, String strJson) throws Exception {
        try {
            MvcResult mvcResult = mockMvc.perform(
                    MockMvcRequestBuilders.get(url)
                            .header("aftersales-token", token_value)
                            .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(strJson)
            )
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andDo(MockMvcResultHandlers.print())
                    .andReturn();
            String responseContent = mvcResult.getResponse().getContentAsString();
            System.out.println("调用返回的结果：" + responseContent);

            // 断言状态码为 200
            assertEquals(200, mvcResult.getResponse().getStatus());
        } catch (Exception e) {
            // 将异常信息打印到控制台，以便调试
            e.printStackTrace();

            if  (e.getCause() instanceof GracefulResponseException) {
                logger.error("捕获到自定义Graceful异常：{}", e.getCause().getMessage());
            }  else if (e.getCause() instanceof ServerErrorException) {
                logger.error("捕获到服务器繁忙异常：{}", e.getCause().getMessage());
            } else if (e.getCause() instanceof BaseCustomException) {
                logger.error("捕获到自定义基础异常：{}", e.getCause().getMessage());
            } else {
                fail("未捕获到预期的异常，捕获到其他异常：" + e.getCause().getMessage());
            }
        }
    }

    public void getMockMvcResult(String url, String paramName, String paramValue) throws Exception {
        try {
            MvcResult mvcResult = mockMvc.perform(
                            MockMvcRequestBuilders.get(url)
                                    .header("aftersales-token", token_value)
                                    .param(paramName,paramValue)
                    )
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andDo(MockMvcResultHandlers.print())
                    .andReturn();
            String responseContent = mvcResult.getResponse().getContentAsString();
            System.out.println("调用返回的结果：" + responseContent);

            // 断言状态码为 200
            assertEquals(200, mvcResult.getResponse().getStatus());
        } catch (Exception e) {
            // 将异常信息打印到控制台，以便调试
            e.printStackTrace();

            if  (e.getCause() instanceof GracefulResponseException) {
                logger.error("捕获到自定义Graceful异常：{}", e.getCause().getMessage());
            }  else if (e.getCause() instanceof ServerErrorException) {
                logger.error("捕获到服务器繁忙异常：{}", e.getCause().getMessage());
            } else if (e.getCause() instanceof BaseCustomException) {
                logger.error("捕获到自定义基础异常：{}", e.getCause().getMessage());
            } else {
                fail("未捕获到预期的异常，捕获到其他异常：" + e.getCause().getMessage());
            }
        }
    }


    public void postMockMvcResult(String url, String strJson) throws Exception {
        try {
            MvcResult mvcResult = mockMvc.perform(
                            MockMvcRequestBuilders.post(url)
                                    .header("aftersales-token", token_value)
                                    .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                    .contentType(MediaType.APPLICATION_JSON)
                                    .content(strJson)
                    )
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andDo(MockMvcResultHandlers.print())
                    .andReturn();

            String responseContent = mvcResult.getResponse().getContentAsString();
            System.out.println("调用返回的结果：" + responseContent);

            // 断言状态码为 200
            assertEquals(200, mvcResult.getResponse().getStatus());
        } catch (Exception e) {
            // 将异常信息打印到控制台，以便调试
            e.printStackTrace();

            if  (e.getCause() instanceof GracefulResponseException) {
                logger.error("捕获到自定义Graceful异常：{}", e.getCause().getMessage());
            }  else if (e.getCause() instanceof ServerErrorException) {
                logger.error("捕获到服务器繁忙异常：{}", e.getCause().getMessage());
            } else if (e.getCause() instanceof BaseCustomException) {
                logger.error("捕获到自定义基础异常：{}", e.getCause().getMessage());
            } else {
                fail("未捕获到预期的异常，捕获到其他异常：" + e.getCause().getMessage());
            }
        }
    }
    public void putMockMvcResult(String url, String strJson) throws Exception {
        try {
            MvcResult mvcResult = mockMvc.perform(
                            MockMvcRequestBuilders.put(url)
                                    .header("aftersales-token", token_value)
                                    .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                    .contentType(MediaType.APPLICATION_JSON)
                                    .content(strJson)
                    )
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andDo(MockMvcResultHandlers.print())
                    .andReturn();

            String responseContent = mvcResult.getResponse().getContentAsString();
            System.out.println("调用返回的结果：" + responseContent);

            // 断言状态码为 200
            assertEquals(200, mvcResult.getResponse().getStatus());
        } catch (Exception e) {
            // 将异常信息打印到控制台，以便调试
            e.printStackTrace();

            if  (e.getCause() instanceof GracefulResponseException) {
                logger.error("捕获到自定义Graceful异常：{}", e.getCause().getMessage());
            }  else if (e.getCause() instanceof ServerErrorException) {
                logger.error("捕获到服务器繁忙异常：{}", e.getCause().getMessage());
            } else if (e.getCause() instanceof BaseCustomException) {
                logger.error("捕获到自定义基础异常：{}", e.getCause().getMessage());
            } else {
                fail("未捕获到预期的异常，捕获到其他异常：" + e.getCause().getMessage());
            }
        }
    }

    public void putMockMvcResult(String url) throws Exception {
        try {
            MvcResult mvcResult = mockMvc.perform(
                            MockMvcRequestBuilders.put(url)
                                    .header("aftersales-token", token_value)
                    )
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andDo(MockMvcResultHandlers.print())
                    .andReturn();

            String responseContent = mvcResult.getResponse().getContentAsString();
            System.out.println("调用返回的结果：" + responseContent);

            // 断言状态码为 200
            assertEquals(200, mvcResult.getResponse().getStatus());
        } catch (Exception e) {
            // 将异常信息打印到控制台，以便调试
            e.printStackTrace();

            if  (e.getCause() instanceof GracefulResponseException) {
                logger.error("捕获到自定义Graceful异常：{}", e.getCause().getMessage());
            }  else if (e.getCause() instanceof ServerErrorException) {
                logger.error("捕获到服务器繁忙异常：{}", e.getCause().getMessage());
            } else if (e.getCause() instanceof BaseCustomException) {
                logger.error("捕获到自定义基础异常：{}", e.getCause().getMessage());
            } else {
                fail("未捕获到预期的异常，捕获到其他异常：" + e.getCause().getMessage());
            }
        }
    }

    public void deleteMockMvcResult(String url) throws Exception {
        try {
            MvcResult mvcResult = mockMvc.perform(
                            MockMvcRequestBuilders.delete(url)
                                    .header("aftersales-token", token_value)
                    )
                    .andExpect(MockMvcResultMatchers.status().isOk())
                    .andDo(MockMvcResultHandlers.print())
                    .andReturn();

            String responseContent = mvcResult.getResponse().getContentAsString();
            System.out.println("调用返回的结果：" + responseContent);

            // 断言状态码为 200
            assertEquals(200, mvcResult.getResponse().getStatus());
        } catch (Exception e) {
            // 将异常信息打印到控制台，以便调试
            e.printStackTrace();

            if  (e.getCause() instanceof GracefulResponseException) {
                logger.error("捕获到自定义Graceful异常：{}", e.getCause().getMessage());
            }  else if (e.getCause() instanceof ServerErrorException) {
                logger.error("捕获到服务器繁忙异常：{}", e.getCause().getMessage());
            } else if (e.getCause() instanceof BaseCustomException) {
                logger.error("捕获到自定义基础异常：{}", e.getCause().getMessage());
            } else {
                fail("未捕获到预期的异常，捕获到其他异常：" + e.getCause().getMessage());
            }
        }
    }

}
