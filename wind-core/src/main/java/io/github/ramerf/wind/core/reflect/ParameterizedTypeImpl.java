package io.github.ramerf.wind.core.reflect;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Objects;
import lombok.Getter;

/**
 * The type Parameterized type.
 *
 * @since 2020.08.23
 * @author Tang Xiaofeng
 */
@Getter
public class ParameterizedTypeImpl implements ParameterizedType {
  private final Type[] actualTypeArguments;
  private final Class<?> rawType;
  private final Type ownerType;

  /**
   * Instantiates a new Parameterized type.
   *
   * @param actualTypeArguments the actual type arguments
   * @param ownerType the owner type
   * @param rawType the raw type
   */
  public ParameterizedTypeImpl(Class<?> rawType, Type[] actualTypeArguments, Type ownerType) {
    this.rawType = rawType;
    this.actualTypeArguments = actualTypeArguments;
    this.ownerType = ownerType != null ? ownerType : rawType.getDeclaringClass();
  }

  public boolean equals(Object var1) {
    if (var1 instanceof ParameterizedType) {
      ParameterizedType var2 = (ParameterizedType) var1;
      if (this == var2) {
        return true;
      } else {
        Type var3 = var2.getOwnerType();
        Type var4 = var2.getRawType();
        return Objects.equals(this.ownerType, var3)
            && Objects.equals(this.rawType, var4)
            && Arrays.equals(this.actualTypeArguments, var2.getActualTypeArguments());
      }
    } else {
      return false;
    }
  }

  public int hashCode() {
    return Arrays.hashCode(this.actualTypeArguments)
        ^ Objects.hashCode(this.ownerType)
        ^ Objects.hashCode(this.rawType);
  }

  public String toString() {
    StringBuilder var1 = new StringBuilder();
    if (this.ownerType != null) {
      if (this.ownerType instanceof Class) {
        var1.append(((Class<?>) this.ownerType).getName());
      } else {
        var1.append(this.ownerType.toString());
      }

      var1.append("$");
      if (this.ownerType instanceof ParameterizedTypeImpl) {
        var1.append(
            this.rawType
                .getName()
                .replace(((ParameterizedTypeImpl) this.ownerType).rawType.getName() + "$", ""));
      } else {
        var1.append(this.rawType.getSimpleName());
      }
    } else {
      var1.append(this.rawType.getName());
    }

    if (this.actualTypeArguments != null && this.actualTypeArguments.length > 0) {
      var1.append("<");
      boolean var2 = true;
      Type[] var3 = this.actualTypeArguments;
      int var4 = var3.length;

      for (Type var6 : var3) {
        if (!var2) {
          var1.append(", ");
        }

        var1.append(var6.getTypeName());
        var2 = false;
      }

      var1.append(">");
    }

    return var1.toString();
  }
}
