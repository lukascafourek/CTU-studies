package cz.cvut.fel.sin.sintest.controller.interceptors;

import jakarta.servlet.http.HttpServletRequest;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope("singleton")
public class ShortLoggingStrategy implements LoggingStrategy {

    @Override
    public void logPreHandle(HttpServletRequest request) {
        log.info("{}, sessionID:{}", request.getRequestURI(), request.getSession().getId());
    }

    @Override
    public void logPostHandle(HttpServletRequest request) {
    }
}
