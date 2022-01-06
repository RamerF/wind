package io.github.ramerf.wind.core.annotation;

@ConfigurationProperties(prefix = "wind")
public class ConfigurationBean {
  private String ddlAuto;
  private String entityPackage;
  @NestConfigurationProperties private DataSourceConfig dataSourceConfig;

  public static class DataSourceConfig {
    private String url;
    private String username;
    private String password;
    private String driverClassName;
  }
}
