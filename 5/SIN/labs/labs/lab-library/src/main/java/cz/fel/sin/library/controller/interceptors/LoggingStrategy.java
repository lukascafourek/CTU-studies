package cz.fel.sin.library.controller.interceptors;

import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public interface LoggingStrategy {

    Logger log = LoggerFactory.getLogger(LoggingStrategy.class);

    void logPreHandle(HttpServletRequest request);

    void logPostHandle(HttpServletRequest request);
}
