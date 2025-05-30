package cz.cvut.fel.omo.hw.functions.utils;

import lombok.experimental.UtilityClass;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;

@UtilityClass
public class CompletableFutureUtils {

    public static <T, U> U applyAndGet(CompletableFuture<T> completableFuture, Function<T, U> applyFnc) {
        try {
            return applyFnc.apply(completableFuture.get());
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException("Error with CompletableFuture", e);
        }
    }
}
