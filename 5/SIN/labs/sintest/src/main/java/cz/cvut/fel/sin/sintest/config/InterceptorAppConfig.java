package cz.cvut.fel.sin.sintest.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import cz.cvut.fel.sin.sintest.controller.interceptors.LoggerInterceptor;

@Component
public class InterceptorAppConfig implements WebMvcConfigurer {

    private final LoggerInterceptor loggerInterceptor;

    @Autowired
    public InterceptorAppConfig(LoggerInterceptor loggerInterceptor) {
        this.loggerInterceptor = loggerInterceptor;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(loggerInterceptor).addPathPatterns("/book/**", "/library/**", "/publisher/**");
    }
}
