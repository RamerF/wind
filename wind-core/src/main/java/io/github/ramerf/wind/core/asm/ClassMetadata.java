package io.github.ramerf.wind.core.asm;

import io.github.ramerf.wind.core.asm.tree.AnnotationNode;
import io.github.ramerf.wind.core.asm.tree.ClassNode;
import io.github.ramerf.wind.core.util.BeanUtils;
import java.lang.annotation.Annotation;
import java.util.*;
import java.util.stream.Collectors;

public class ClassMetadata {
  /** 注解类全限定名 */
  private final List<String> annotations;

  private final String name;

  public ClassMetadata(final ClassNode classNode) {
    final String name = classNode.name.replace("/", ".");
    final List<AnnotationNode> annotations = classNode.visibleAnnotations;
    this.name = name;
    this.annotations =
        annotations == null
            ? Collections.emptyList()
            : annotations.stream()
                .map(o -> Type.getType(o.desc))
                .map(Type::getClassName)
                .collect(Collectors.toList());
  }

  public Set<Class<Annotation>> getAnnotationSet() {
    Set<Class<Annotation>> annotationSet = new HashSet<>();
    for (final String anno : annotations) {
      final Class<Annotation> clazz = BeanUtils.getClazz(anno);
      if (Annotation.class.isAssignableFrom(clazz)) {
        annotationSet.add(clazz);
      }
    }
    return annotationSet;
  }

  public Class<?> getCurrentClass() {
    return BeanUtils.getClazz(name);
  }
}
