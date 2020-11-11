package io.github.ramerf.wind.core.generator;

import com.google.auto.service.AutoService;
import com.sun.tools.javac.api.JavacTrees;
import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.processing.JavacProcessingEnvironment;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.JCTree.*;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.util.*;
import io.github.ramerf.wind.core.entity.enums.InterEnum;
import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.util.HashSet;
import java.util.Set;
import javax.annotation.processing.*;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.tools.Diagnostic;

@AutoService(Processor.class)
// @SupportedSourceVersion(SourceVersion.RELEASE_8)
// @SupportedAnnotationTypes("io.github.ramerf.wind.annotation.CommonEnum")
public class InterEnumFieldGenerator extends AbstractProcessor {

  public void handle(AnnotationValues annotationValues, JCTree ast, APTBuilder aptBuilder) {
    messager.printMessage(
        Diagnostic.Kind.NOTE,
        "printMessage: " + treeMaker.Parens(aptBuilder.typeRef(Serializable.class)));
    if (ast == null) return;
    final CommonEnum commonEnum = annotationValues.getAnnotationValue(CommonEnum.class);
    final Class<?> valueType = commonEnum.valueType();

    TreeMaker treeMaker = aptBuilder.getTreeMaker();
    //  | Flags.FINAL
    JCModifiers modifiers = treeMaker.Modifiers(Flags.PUBLIC | Flags.STATIC);
    // modifiers
    JCVariableDecl descVar = aptBuilder.newVar(modifiers.flags, String.class, "desc", null);
    aptBuilder.inject(descVar);
    final JCVariableDecl valueVar =
        treeMaker.VarDef(
            modifiers, aptBuilder.toName("value"), aptBuilder.typeRef(valueType), null);
    aptBuilder.inject(valueVar);
    // aptBuilder.classRef();
    // treeMaker.Modifiers(Flags.INTERFACE);
    // 方法
    // JCExpression fieldRef =
    //     treeMaker.Select(treeMaker.Ident(names.fromString("this")), names.fromString("value"));
    //
    // JCStatement returnStatement =
    //     treeMaker.Return(
    //         treeMaker.Select(treeMaker.Ident(names.fromString("this")),
    // names.fromString("value")));
    // aptBuilder.inject(
    //     treeMaker.MethodDef(
    //         treeMaker.Modifiers(Flags.PUBLIC | Flags.FINAL),
    //         names.fromString("value"),
    //         fieldRef,
    //         List.nil(),
    //         List.nil(),
    //         List.nil(),
    //         treeMaker.Block(0, List.of(returnStatement)),
    //         null));
    aptBuilder.inject(aptBuilder.newMethod(valueVar, "value", "value"));
    aptBuilder.inject(aptBuilder.newMethod(descVar, "desc", "desc"));
    // implement InterEnum
    final ListBuffer<JCExpression> listBuffer =
        ListBuffer.of(aptBuilder.newGenericsType(InterEnum.class, valueType));
    listBuffer.addAll(aptBuilder.classDecl.implementing);
    aptBuilder.classDecl.implementing = listBuffer.toList();

    JCExpression fieldRef = treeMaker.Select(aptBuilder.varRef("this"), valueVar.name);
    treeMaker.Assign(fieldRef, treeMaker.Ident(valueVar.name));
    JCVariableDecl valueParam =
        treeMaker.VarDef(
            treeMaker.Modifiers(Flags.FINAL | Flags.PARAMETER),
            valueVar.name,
            valueVar.vartype,
            null);
    JCVariableDecl descParam =
        treeMaker.VarDef(
            treeMaker.Modifiers(Flags.FINAL | Flags.PARAMETER),
            descVar.name,
            descVar.vartype,
            null);
    StatementBuilder constructorStatement = aptBuilder.createStatementBuilder();
    final JCClassDecl classDecl = aptBuilder.classDecl;
    // 去掉默认构造器
    ListBuffer<JCTree> defs = new ListBuffer<>();
    final JCMethodDecl constructor = createEmptyConstruct(aptBuilder);
    for (int i = 0; i < classDecl.defs.size(); i++) {
      final JCTree def = classDecl.defs.get(i);
      messager.printMessage(Diagnostic.Kind.NOTE, "printMessage: +++++" + def);
      if (!def.toString().equals(constructor.toString())) {
        defs.append(def);
      } else {
        messager.printMessage(Diagnostic.Kind.NOTE, "printMessage: ___________" + def);
      }
    }
    defs.toList()
        .forEach(def -> messager.printMessage(Diagnostic.Kind.NOTE, "printMessage: " + def));
    classDecl.defs =
        defs.toList()
            .append(
                aptBuilder.createConstructor(
                    Flags.PRIVATE, List.of(valueParam, descParam), constructorStatement.build()));
  }

  private JCMethodDecl createEmptyConstruct(APTBuilder aptBuilder) {
    // 去掉默认构造器
    final StatementBuilder builder = aptBuilder.createStatementBuilder();
    builder.append("super");
    return aptBuilder.createConstructor(Flags.PRIVATE, List.nil(), builder.build());
  }

  /*PRE.*/

  private Messager messager;
  private Elements elementUtils;
  private JavacTrees javacTrees;
  private TreeMaker treeMaker;
  private Names names;
  private ClassLoader classloader;

  @Override
  public synchronized void init(ProcessingEnvironment processingEnv) {
    super.init(processingEnv);

    this.classloader = ClassLoader.getSystemClassLoader();
    this.messager = processingEnv.getMessager();
    this.elementUtils = processingEnv.getElementUtils();
    this.javacTrees = JavacTrees.instance(processingEnv);
    Context context = ((JavacProcessingEnvironment) processingEnv).getContext();
    this.treeMaker = TreeMaker.instance(context);
    this.names = Names.instance(context);
    messager.printMessage(Diagnostic.Kind.NOTE, ">>>>>>>>>>>>>>>>>>>>>>>>>");
  }

  @Override
  public Set<String> getSupportedAnnotationTypes() {
    final Set<String> set = new HashSet<>();
    set.add(CommonEnum.class.getCanonicalName());
    return set;
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    final Class<? extends Annotation> annotationClass = CommonEnum.class;
    final Set<? extends Element> elements = roundEnv.getElementsAnnotatedWith(annotationClass);
    for (Element element : elements) {
      JCTree ast = javacTrees.getTree(element);
      JCTree.JCClassDecl classDecl;
      if (ast instanceof JCTree.JCClassDecl) {
        classDecl = (JCTree.JCClassDecl) ast;
      } else if (ast instanceof JCTree.JCVariableDecl) {
        classDecl = getClassDecl((JCTree.JCVariableDecl) ast);
      } else if (ast instanceof JCTree.JCMethodDecl) {
        classDecl = getClassDecl((JCTree.JCMethodDecl) ast);
      } else {
        classDecl = null;
      }
      // final ClassSymbol symbol = new ClassSymbol();
      // classDecl.sym = ;
      APTBuilder aptBuilder = new APTBuilder(classDecl, element, ast, treeMaker, names, messager);
      handle(new AnnotationValues(ast, classloader), ast, aptBuilder);
    }
    return true;
  }

  private JCTree.JCClassDecl getClassDecl(JCTree.JCVariableDecl tree) {
    String className = tree.sym.owner.getQualifiedName().toString();
    TypeElement typeElement = elementUtils.getTypeElement(className);
    return javacTrees.getTree(typeElement);
  }

  private JCTree.JCClassDecl getClassDecl(JCTree.JCMethodDecl tree) {
    String className = tree.sym.owner.getQualifiedName().toString();
    TypeElement typeElement = elementUtils.getTypeElement(className);
    return javacTrees.getTree(typeElement);
  }
}
