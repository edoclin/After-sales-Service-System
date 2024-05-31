package com.mi.aftersales.config;

import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

/**
 * @author QYenon
 * @create 2024/5/31
 */
@Component
public class TestConfig {

    private static final String token_value = "Bearer Tj_MBae2PQ1d06DzG_wJxS12eECWcGcwUs__";



    private MockMvc mockMvc;

    public MockMvc getMockMvc() {
        return mockMvc;
    }

    public void setMockMvc(MockMvc mockMvc) {
        this.mockMvc = mockMvc;
    }

    public MvcResult getMockMvcResult(String url) throws Exception {
        return mockMvc.perform(
                        MockMvcRequestBuilders.get(url)
                                .header("aftersales-token", token_value)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
    }

    public MvcResult getMockMvcResult(String url, String strJson) throws Exception {
        return mockMvc.perform(
                        MockMvcRequestBuilders.get(url)
                                .header("aftersales-token", token_value)
                                .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(strJson)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
    }

    public MvcResult getMockMvcResult(String url, String paramName, String paramValue) throws Exception {
        return mockMvc.perform(
                        MockMvcRequestBuilders.get(url)
                                .header("aftersales-token", token_value)
                                .param(paramName,paramValue)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
    }


    public MvcResult postMockMvcResult(String url, String strJson) throws Exception {
        return mockMvc.perform(
                        MockMvcRequestBuilders.post(url)
                                .header("aftersales-token", token_value)
                                .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(strJson)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
    }
    public MvcResult putMockMvcResult(String url, String strJson) throws Exception {
        return mockMvc.perform(
                        MockMvcRequestBuilders.put(url)
                                .header("aftersales-token", token_value)
                                .accept(MediaType.parseMediaType("application/json;charset=UTF-8"))
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(strJson)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
    }

    public MvcResult putMockMvcResult(String url) throws Exception {
        return mockMvc.perform(
                        MockMvcRequestBuilders.put(url)
                                .header("aftersales-token", token_value)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
    }

    public MvcResult deleteMockMvcResult(String url) throws Exception {
        return mockMvc.perform(
                        MockMvcRequestBuilders.delete(url)
                                .header("aftersales-token", token_value)
                )
                .andExpect(MockMvcResultMatchers.status().isOk())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();
    }

}
