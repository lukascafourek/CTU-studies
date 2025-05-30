package cz.cvut.fel.omo.hw.functions;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.*;
import com.github.javaparser.ast.expr.LambdaExpr;
import com.github.javaparser.ast.expr.MethodReferenceExpr;
import com.github.javaparser.ast.expr.VariableDeclarationExpr;
import com.github.javaparser.ast.stmt.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

class JavaCodeStatementsTest {

    private final String BASE_PATH = "src/main/java/cz/cvut/fel/omo/hw/functions";
    private final Set<String> FUNCTIONAL_PATHS = Set.of(BASE_PATH + "/statistics", BASE_PATH + "/utils");
    private final Set<String> FUNCTIONAL_INTERFACES = Set.of("Predicate", "Consumer", "Function", "BiFunction", "Supplier", "UnaryOperator", "BinaryOperator", "Comparator", "Runnable", "Callable");

    @Test
    @DisplayName("Test if code contains blacklisted statements.")
    void sourcesDoNotContainsBlacklistedStatements() throws IOException {
        Files.walkFileTree(Path.of(BASE_PATH), new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                    throws IOException {
                CompilationUnit cu = StaticJavaParser.parse(file);
                Assertions.assertEquals(0, cu.findAll(ForStmt.class).size(), "Code contains 'for' statement!");
                Assertions.assertEquals(0, cu.findAll(ForEachStmt.class).size(), "Code contains 'foreach' statement!");
                Assertions.assertEquals(0, cu.findAll(WhileStmt.class).size(), "Code contains 'while' statement!");
                Assertions.assertEquals(0, cu.findAll(DoStmt.class).size(), "Code contains 'do-while' statement!");
                Assertions.assertEquals(0, cu.findAll(IfStmt.class).size(), "Code contains 'if' statement!");
                Assertions.assertEquals(0, cu.findAll(SwitchStmt.class).size(), "Code contains 'switch' statement!");
                return FileVisitResult.CONTINUE;
            }
        });
    }

    @Test
    @DisplayName("Test if code contains functional expressions.")
    void sourcesContainsFunctionalExpressions() throws IOException {
        for (String functionalPath : FUNCTIONAL_PATHS) {
            Files.walkFileTree(Path.of(functionalPath), new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                        throws IOException {
                    CompilationUnit cu = StaticJavaParser.parse(file);
                    if (Optional.ofNullable(cu.getType(0)).map(BodyDeclaration::asClassOrInterfaceDeclaration).map(ClassOrInterfaceDeclaration::isInterface).orElse(false)) {
                        return FileVisitResult.CONTINUE;
                    }

                    int lambdaCount = cu.findAll(LambdaExpr.class).size();
                    int fncRefCount = cu.findAll(MethodReferenceExpr.class).size();
                    long highOrderFunctionCount = cu.findAll(MethodDeclaration.class).stream()
                            .map(CallableDeclaration::getParameters)
                            .flatMap(Collection::stream)
                            .map(Parameter::getTypeAsString)
                            .filter(s -> FUNCTIONAL_INTERFACES.stream().anyMatch(s::startsWith))
                            .count();

                    Assertions.assertTrue(lambdaCount + fncRefCount + highOrderFunctionCount > 0, "Each class must contain 'lambda' or 'method reference' expression or usage 'high-order-function'!");
                    return FileVisitResult.CONTINUE;
                }

            });
        }
    }

    @Test
    @DisplayName("Test if code contains only few variable declarations.")
    void sourcesContainsOnlyFewVariableDeclarations() throws IOException {
        final AtomicLong allVarDeclaration = new AtomicLong(0);
        for (String functionalPath : FUNCTIONAL_PATHS) {
            Files.walkFileTree(Path.of(functionalPath), new SimpleFileVisitor<>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                        throws IOException {
                    CompilationUnit cu = StaticJavaParser.parse(file);
                    if (Optional.ofNullable(cu.getType(0)).map(BodyDeclaration::asClassOrInterfaceDeclaration).map(ClassOrInterfaceDeclaration::isInterface).orElse(false)) {
                        return FileVisitResult.CONTINUE;
                    }

                    allVarDeclaration.addAndGet(cu.findAll(VariableDeclarationExpr.class).size());
                    return FileVisitResult.CONTINUE;
                }
            });
        }

        Assertions.assertTrue(allVarDeclaration.get() <= 5, "You can use 5 variable declarations at maximum in your program!");
    }

}
